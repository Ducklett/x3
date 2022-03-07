const path = require('path')
const fs = require('fs')

function read(filename) { return fs.readFileSync(filename, 'utf-8') }
function write(filename, content) { return fs.writeFileSync(filename, content) }
function assert(expr, msg = '') {
    if (expr)
        return;
    const stack = new Error().stack;
    const failedAt = stack.split('\n')[2];
    const [_, file, line] = failedAt.match(/\((.*):(\d+:\d+)\)/);
    const relativeToRoot = path.relative(__dirname, file);
    const displayMessage = msg ? `: '${msg}'` : '';
    console.log(`${relativeToRoot}(${line}) assertion failed${displayMessage}`);
    process.exit(0);
}

const api = {
    read,
    write,
    assert,

    fd: {
        stdin: 0,
        stdout: 1,
        stderr: 2,
    },

    fn: (name, instructions) => ({ kind: 'function', name, instructions }),
    call: (name, ...args) => ({ kind: 'call', name, args }),
    syscall: (name, ...args) => ({ kind: 'syscall', name, args }),
    declareVar: (name) => ({ kind: 'declareVar', name }),
    assignVar: (varDec, expr) => ({ kind: 'assignVar', varDec, expr }),
    readVar: (varDec) => ({ kind: 'readVar', varDec }),
    add: (a, b) => ({ kind: 'add', a, b }),
    num: (n) => ({ kind: 'numberLiteral', n }),

    emitAsm(ast, dest = 'out/out.asm') {
        let locals;
        // data => label lookup
        const datalut = new Map();
        // label => data lookup
        const data = new Map();
        const label = (content) => {
            const foundL = datalut.get(content);
            if (foundL)
                return foundL;
            if (!global.labelIndex)
                global.labelIndex = 0;
            global.labelIndex++;
            const l = `__label${global.labelIndex}`;
            data.set(l, content);
            datalut.set(content, l);
            return l;
        };
        const lines = [];
        let hasMain = false;
        function emitTop(node) {
            switch (node.kind) {
                case 'function': {
                    global.varIndex = 0;
                    locals = new Map();
                    // TODO: maybe some kind of mangling so we can also have a function called start?
                    let name = node.name;
                    const isMain = name === 'main';
                    if (isMain) {
                        hasMain = true;
                        name = 'start'
                    }


                    lines.push(`_${name}:`);
                    for (let expr of node.instructions)
                        emitExpr(expr);
                    if (!isMain)
                        lines.push('ret');
                    return;
                }
                default: assert(false, `unexpected top level kind ${node.kind}`);
            }
        }
        function emitExpr(node) {
            switch (node.kind) {
                case 'syscall': {
                    const nr = syscalls[node.name];
                    assert(nr !== undefined, 'must be a linux syscall');
                    lines.push(`; ${node.name} syscall`);
                    lines.push(`mov rax, ${nr}`);
                    const argRegisters = ['rdi', 'rsi', 'rdx', 'r10', 'r8', 'r9'];
                    for (let i = 0; i < node.args.length; i++) {
                        const arg = node.args[i];
                        if (typeof arg === 'string') {
                            lines.push(`mov ${argRegisters[i]}, ${label(arg)}`);
                        }
                        else if (typeof arg === 'number') {
                            lines.push(`mov ${argRegisters[i]}, ${arg}`);
                        }
                        else {
                            assert(typeof arg === 'object', 'must be object');
                            assert(arg.kind === 'readVar', ' only var is supported');
                            lines.push(`mov ${argRegisters[i]}, ${locals.get(arg.varDec)}`);
                        }
                    }
                    lines.push('syscall');
                    lines.push('');
                    return;
                }
                case 'call': {
                    assert(!node.args || node.args.length === 0, 'function arguments not supported');
                    lines.push(`call _${node.name}`);
                    return;
                }
                case 'declareVar': {
                    const varRegisters = ['rbx', 'rcx', 'r11', 'r12', 'r13', 'r14', 'r15'];
                    if (global.varIndex === undefined)
                        global.varIndex = 0;
                    locals.set(node, varRegisters[global.varIndex++]);
                    return;
                }
                case 'add': {
                    assert(node.a.kind == 'readVar', 'add lhs is var');
                    assert(node.b.kind == 'readVar', 'add rhs is var');
                    // by storing the result in rax we preseve the values of the variables
                    // move a into rax
                    lines.push(`mov rax, ${locals.get(node.a.varDec)}`);
                    // add b into rax
                    lines.push(`add rax, ${locals.get(node.b.varDec)}`);
                    return;
                }
                case 'numberLiteral': {
                    // TODO: get a fitting empty register instead of hardcoded rax
                    lines.push(`mov rax, ${node.n}`);
                    return;
                }
                case 'assignVar': {
                    console.assert(typeof node.varDec === 'object', 'vardef must be object');
                    const theVar = locals.get(node.varDec);
                    assert(theVar !== undefined, `the variable ${node.varDec.name} has been declared`);
                    if (node.expr.kind == 'numberLiteral') {
                        lines.push(`mov ${theVar}, ${node.expr.n}`);
                    }
                    else {
                        emitExpr(node.expr);
                        // TODO: get the register of the expression and use that instead of rax
                        lines.push(`mov ${theVar}, rax`);
                    }
                    return;
                }
                default: assert(false, `unsupported node kind ${node.kind}`);
            }
        }
        for (let node of ast)
            emitTop(node);
        assert(hasMain, 'the program defines a main function');
        const source = `
section .text
global _start
${lines.join('\n')}
section .data
${[...data.keys()].map(k => {
            const msg = data.get(k);
            assert(typeof msg === 'string', 'only string data is supported');
            const bytes = [...msg].map(c => c.charCodeAt(0)).join(',');
            return `${k} db ${bytes} ; '${msg.replace(/\n/g, '\\n')}'`;
        }).join('\n')}
`;
        write(dest, source);
    }
}

// https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md#x86_64-64_bit
/**
console.log([...document.querySelectorAll('table')[1]
.querySelectorAll('tr')]
.filter((v,i)=>i)
.map(tr => {
    const tds = tr.querySelectorAll('td')
    return tds[1].textContent + ':' + tds[3].textContent +','
}).join('\n'))
 */
const syscalls = {
    read: 0x00,
    write: 0x01,
    open: 0x02,
    close: 0x03,
    stat: 0x04,
    fstat: 0x05,
    lstat: 0x06,
    poll: 0x07,
    lseek: 0x08,
    mmap: 0x09,
    mprotect: 0x0a,
    munmap: 0x0b,
    brk: 0x0c,
    rt_sigaction: 0x0d,
    rt_sigprocmask: 0x0e,
    rt_sigreturn: 0x0f,
    ioctl: 0x10,
    pread64: 0x11,
    pwrite64: 0x12,
    readv: 0x13,
    writev: 0x14,
    access: 0x15,
    pipe: 0x16,
    select: 0x17,
    sched_yield: 0x18,
    mremap: 0x19,
    msync: 0x1a,
    mincore: 0x1b,
    madvise: 0x1c,
    shmget: 0x1d,
    shmat: 0x1e,
    shmctl: 0x1f,
    dup: 0x20,
    dup2: 0x21,
    pause: 0x22,
    nanosleep: 0x23,
    getitimer: 0x24,
    alarm: 0x25,
    setitimer: 0x26,
    getpid: 0x27,
    sendfile: 0x28,
    socket: 0x29,
    connect: 0x2a,
    accept: 0x2b,
    sendto: 0x2c,
    recvfrom: 0x2d,
    sendmsg: 0x2e,
    recvmsg: 0x2f,
    shutdown: 0x30,
    bind: 0x31,
    listen: 0x32,
    getsockname: 0x33,
    getpeername: 0x34,
    socketpair: 0x35,
    setsockopt: 0x36,
    getsockopt: 0x37,
    clone: 0x38,
    fork: 0x39,
    vfork: 0x3a,
    execve: 0x3b,
    exit: 0x3c,
    wait4: 0x3d,
    kill: 0x3e,
    uname: 0x3f,
    semget: 0x40,
    semop: 0x41,
    semctl: 0x42,
    shmdt: 0x43,
    msgget: 0x44,
    msgsnd: 0x45,
    msgrcv: 0x46,
    msgctl: 0x47,
    fcntl: 0x48,
    flock: 0x49,
    fsync: 0x4a,
    fdatasync: 0x4b,
    truncate: 0x4c,
    ftruncate: 0x4d,
    getdents: 0x4e,
    getcwd: 0x4f,
    chdir: 0x50,
    fchdir: 0x51,
    rename: 0x52,
    mkdir: 0x53,
    rmdir: 0x54,
    creat: 0x55,
    link: 0x56,
    unlink: 0x57,
    symlink: 0x58,
    readlink: 0x59,
    chmod: 0x5a,
    fchmod: 0x5b,
    chown: 0x5c,
    fchown: 0x5d,
    lchown: 0x5e,
    umask: 0x5f,
    gettimeofday: 0x60,
    getrlimit: 0x61,
    getrusage: 0x62,
    sysinfo: 0x63,
    times: 0x64,
    ptrace: 0x65,
    getuid: 0x66,
    syslog: 0x67,
    getgid: 0x68,
    setuid: 0x69,
    setgid: 0x6a,
    geteuid: 0x6b,
    getegid: 0x6c,
    setpgid: 0x6d,
    getppid: 0x6e,
    getpgrp: 0x6f,
    setsid: 0x70,
    setreuid: 0x71,
    setregid: 0x72,
    getgroups: 0x73,
    setgroups: 0x74,
    setresuid: 0x75,
    getresuid: 0x76,
    setresgid: 0x77,
    getresgid: 0x78,
    getpgid: 0x79,
    setfsuid: 0x7a,
    setfsgid: 0x7b,
    getsid: 0x7c,
    capget: 0x7d,
    capset: 0x7e,
    rt_sigpending: 0x7f,
    rt_sigtimedwait: 0x80,
    rt_sigqueueinfo: 0x81,
    rt_sigsuspend: 0x82,
    sigaltstack: 0x83,
    utime: 0x84,
    mknod: 0x85,
    uselib: 0x86,
    personality: 0x87,
    ustat: 0x88,
    statfs: 0x89,
    fstatfs: 0x8a,
    sysfs: 0x8b,
    getpriority: 0x8c,
    setpriority: 0x8d,
    sched_setparam: 0x8e,
    sched_getparam: 0x8f,
    sched_setscheduler: 0x90,
    sched_getscheduler: 0x91,
    sched_get_priority_max: 0x92,
    sched_get_priority_min: 0x93,
    sched_rr_get_interval: 0x94,
    mlock: 0x95,
    munlock: 0x96,
    mlockall: 0x97,
    munlockall: 0x98,
    vhangup: 0x99,
    modify_ldt: 0x9a,
    pivot_root: 0x9b,
    _sysctl: 0x9c,
    prctl: 0x9d,
    arch_prctl: 0x9e,
    adjtimex: 0x9f,
    setrlimit: 0xa0,
    chroot: 0xa1,
    sync: 0xa2,
    acct: 0xa3,
    settimeofday: 0xa4,
    mount: 0xa5,
    umount2: 0xa6,
    swapon: 0xa7,
    swapoff: 0xa8,
    reboot: 0xa9,
    sethostname: 0xaa,
    setdomainname: 0xab,
    iopl: 0xac,
    ioperm: 0xad,
    create_module: 0xae,
    init_module: 0xaf,
    delete_module: 0xb0,
    get_kernel_syms: 0xb1,
    query_module: 0xb2,
    quotactl: 0xb3,
    nfsservctl: 0xb4,
    getpmsg: 0xb5,
    putpmsg: 0xb6,
    afs_syscall: 0xb7,
    tuxcall: 0xb8,
    security: 0xb9,
    gettid: 0xba,
    readahead: 0xbb,
    setxattr: 0xbc,
    lsetxattr: 0xbd,
    fsetxattr: 0xbe,
    getxattr: 0xbf,
    lgetxattr: 0xc0,
    fgetxattr: 0xc1,
    listxattr: 0xc2,
    llistxattr: 0xc3,
    flistxattr: 0xc4,
    removexattr: 0xc5,
    lremovexattr: 0xc6,
    fremovexattr: 0xc7,
    tkill: 0xc8,
    time: 0xc9,
    futex: 0xca,
    sched_setaffinity: 0xcb,
    sched_getaffinity: 0xcc,
    set_thread_area: 0xcd,
    io_setup: 0xce,
    io_destroy: 0xcf,
    io_getevents: 0xd0,
    io_submit: 0xd1,
    io_cancel: 0xd2,
    get_thread_area: 0xd3,
    lookup_dcookie: 0xd4,
    epoll_create: 0xd5,
    epoll_ctl_old: 0xd6,
    epoll_wait_old: 0xd7,
    remap_file_pages: 0xd8,
    getdents64: 0xd9,
    set_tid_address: 0xda,
    restart_syscall: 0xdb,
    semtimedop: 0xdc,
    fadvise64: 0xdd,
    timer_create: 0xde,
    timer_settime: 0xdf,
    timer_gettime: 0xe0,
    timer_getoverrun: 0xe1,
    timer_delete: 0xe2,
    clock_settime: 0xe3,
    clock_gettime: 0xe4,
    clock_getres: 0xe5,
    clock_nanosleep: 0xe6,
    exit_group: 0xe7,
    epoll_wait: 0xe8,
    epoll_ctl: 0xe9,
    tgkill: 0xea,
    utimes: 0xeb,
    vserver: 0xec,
    mbind: 0xed,
    set_mempolicy: 0xee,
    get_mempolicy: 0xef,
    mq_open: 0xf0,
    mq_unlink: 0xf1,
    mq_timedsend: 0xf2,
    mq_timedreceive: 0xf3,
    mq_notify: 0xf4,
    mq_getsetattr: 0xf5,
    kexec_load: 0xf6,
    waitid: 0xf7,
    add_key: 0xf8,
    request_key: 0xf9,
    keyctl: 0xfa,
    ioprio_set: 0xfb,
    ioprio_get: 0xfc,
    inotify_init: 0xfd,
    inotify_add_watch: 0xfe,
    inotify_rm_watch: 0xff,
    migrate_pages: 0x100,
    openat: 0x101,
    mkdirat: 0x102,
    mknodat: 0x103,
    fchownat: 0x104,
    futimesat: 0x105,
    newfstatat: 0x106,
    unlinkat: 0x107,
    renameat: 0x108,
    linkat: 0x109,
    symlinkat: 0x10a,
    readlinkat: 0x10b,
    fchmodat: 0x10c,
    faccessat: 0x10d,
    pselect6: 0x10e,
    ppoll: 0x10f,
    unshare: 0x110,
    set_robust_list: 0x111,
    get_robust_list: 0x112,
    splice: 0x113,
    tee: 0x114,
    sync_file_range: 0x115,
    vmsplice: 0x116,
    move_pages: 0x117,
    utimensat: 0x118,
    epoll_pwait: 0x119,
    signalfd: 0x11a,
    timerfd_create: 0x11b,
    eventfd: 0x11c,
    fallocate: 0x11d,
    timerfd_settime: 0x11e,
    timerfd_gettime: 0x11f,
    accept4: 0x120,
    signalfd4: 0x121,
    eventfd2: 0x122,
    epoll_create1: 0x123,
    dup3: 0x124,
    pipe2: 0x125,
    inotify_init1: 0x126,
    preadv: 0x127,
    pwritev: 0x128,
    rt_tgsigqueueinfo: 0x129,
    perf_event_open: 0x12a,
    recvmmsg: 0x12b,
    fanotify_init: 0x12c,
    fanotify_mark: 0x12d,
    prlimit64: 0x12e,
    name_to_handle_at: 0x12f,
    open_by_handle_at: 0x130,
    clock_adjtime: 0x131,
    syncfs: 0x132,
    sendmmsg: 0x133,
    setns: 0x134,
    getcpu: 0x135,
    process_vm_readv: 0x136,
    process_vm_writev: 0x137,
    kcmp: 0x138,
    finit_module: 0x139,
    sched_setattr: 0x13a,
    sched_getattr: 0x13b,
    renameat2: 0x13c,
    seccomp: 0x13d,
    getrandom: 0x13e,
    memfd_create: 0x13f,
    kexec_file_load: 0x140,
    bpf: 0x141,
    execveat: 0x142,
    userfaultfd: 0x143,
    membarrier: 0x144,
    mlock2: 0x145,
    copy_file_range: 0x146,
    preadv2: 0x147,
    pwritev2: 0x148,
    pkey_mprotect: 0x149,
    pkey_alloc: 0x14a,
    pkey_free: 0x14b,
    statx: 0x14c,
};

module.exports = api
