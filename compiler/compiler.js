const path = require('path')
const fs = require('fs')

function read (filename) {
  return fs.readFileSync(filename, 'utf-8')
}
function write (filename, content) {
  return fs.writeFileSync(filename, content)
}
function assert (expr, msg = '') {
  if (expr) return
  const stack = new Error().stack
  const failedAt = stack.split('\n')[2]
  const matched =
    failedAt.match(/\((.*):(\d+:\d+)\)/) ?? failedAt.match(/at (.*):(\d+:\d+)/)
  if (matched === null) {
    console.error('stack parsing error in assert, update it!')
    process.exit(1)
  }
  const [_, file, line] = matched
  const relativeToRoot = path.relative(process.cwd(), file)
  const displayMessage = msg ? `: '${msg}'` : ''
  console.log(`${relativeToRoot}:${line} assertion failed${displayMessage}`)
  process.exit(0)
}

const api = {
  read,
  write,
  assert,

  fd: {
    stdin: 0,
    stdout: 1,
    stderr: 2
  },

  MARK: (...notes) => node => ({ ...node, notes: new Set(notes) }),
  fn: (name, params, instructions) => ({
    kind: 'function',
    name,
    params,
    instructions
  }),
  If: (cond, then, els = null) => ({ kind: 'if', cond, then, els }),
  call: (def, ...args) => ({ kind: 'call', def, args }),
  syscall: (code, ...args) => ({ kind: 'syscall', code, args }),
  param: name => ({ kind: 'param', name }),
  declareVar: (name, expr) => ({ kind: 'declareVar', name, expr }),
  assignVar: (varDec, expr) => ({ kind: 'assignVar', varDec, expr }),
  readVar: varDec => ({ kind: 'readVar', varDec }),
  readProp: (varDec, prop) => ({ kind: 'readProp', varDec, prop }),
  binary: (op, a, b) => ({ kind: 'binary', op, a, b }),
  unary: (op, a) => ({ kind: 'unary', op, a }),
  ret: expr => ({ kind: 'return', expr }),

  num: n => ({ kind: 'numberLiteral', n }),
  str: value => ({ kind: 'stringLiteral', value, len: value.length }),

  emitAsm (ast, dest = 'out/out.asm') {
    let locals

    // variable => label lookup
    const globals = new Map()

    // data => label lookup
    const datalut = new Map()
    // label => data lookup
    const data = new Map()
    const label = (content = null) => {
      if (!global.labelIndex) global.labelIndex = 0

      if (content !== null) {
        const foundL = datalut.get(content)
        if (foundL) return foundL
        global.labelIndex++
        const l = `__label${global.labelIndex}`
        data.set(l, content)
        datalut.set(content, l)
        return l
      } else {
        global.labelIndex++
        const l = `__label${global.labelIndex}`
        return l
      }
    }
    const lines = []
    let hasMain = false
    let returnLabel = null
    function emitVar (node) {
      assert(
        node.kind == 'declareVar' || node.kind == 'param',
        `expected declareVar, got ${node.kind}`
      )
      let offset = locals.get(node)
      if (offset) {
        assert(offset !== undefined)
        if (offset == 0) return '[rbp]'
        return `[rbp${offset > 0 ? '+' : ''}${offset}]`
      } else {
        offset = globals.get(node)
        assert(
          offset !== undefined,
          'referenced variable will either be a local or a global'
        )
        return offset
      }
    }
    function emitTop (node) {
      switch (node.kind) {
        case 'declareVar': {
          const notes = node.notes ?? new Set()
          assert(notes.has('const'), 'top level variables are constant')
          assert(node.expr, 'top level variable is intialized')

          switch (node.expr.kind) {
            case 'stringLiteral': {
              const l = label(node.expr.value)
              globals.set(node, l)
              break
            }
            case 'numberLiteral': {
              const l = label(node.expr.n)
              globals.set(node, l)
              break
            }
            default:
              assert(false, `illegal variable expression ${node.kind}`)
          }

          return
        }
        case 'function': {
          // extern declaration; don't emit
          if (node.notes.has('syscall')) return

          assert(node.instructions, `functions have a body`)

          let optimizations = node.notes ?? new Set()
          let removeAlloc = optimizations.has('doesNotAllocate')

          returnLabel = label()
          locals = new Map()
          let name = node.name
          const isMain = name === 'main'
          if (isMain) {
            hasMain = true
            name = 'start'
          }

          const vars = node.instructions.filter(n => n.kind === 'declareVar')
          const exprs = node.instructions.filter(n => n.kind !== 'declareVar')

          lines.push(`_${name}:`)

          const localSize = vars.length * 8

          lines.push(`; prologue`)
          lines.push(`push rbp`)
          lines.push(`mov rbp, rsp`)
          if (!removeAlloc) lines.push(`sub rsp, ${localSize}\n`)

          let paramOffset = 16 + node.params.length * 8
          for (let param of node.params) {
            paramOffset -= 8
            locals.set(param, paramOffset)
            console.log(`param ${param.name} = [rbp+${paramOffset}]`)
          }

          let varOffset = 0
          for (let vr of vars) {
            if (vr.notes.has('const')) {
              // hoist that bitch!
              emitTop(vr)
              console.log(`const ${vr.name} = ${emitVar(vr)}`)
            } else {
              assert(vr.expr == null, 'initalized locals not supported')
              varOffset -= 8
              locals.set(vr, varOffset)
              console.log(`var ${vr.name} = ${emitVar(vr)}`)
            }
          }

          lines.push(`; body`)
          for (let [expr, i] of exprs.map((v, i) => [v, i])) {
            emitExpr(expr, {
              shouldReturn: false,
              lastOfFunc: i == exprs.length - 1
            })
          }

          lines.push(`${returnLabel}:`)
          lines.push(`; epilogue`)
          lines.push(`mov rsp, rbp`)
          lines.push(`pop rbp`)
          lines.push('ret')
          return
        }
        default:
          assert(false, `unexpected top level kind ${node.kind}`)
      }
    }
    function emitExpr (node, { shouldReturn = true, lastOfFunc = false } = {}) {
      switch (node.kind) {
        case 'return': {
          // note that return WILL push the expr onto the stack
          if (node.expr) {
            emitExpr(node.expr)
            lines.push(`pop rax`)
          }
          lines.push(`; return`)
          lines.push(`jmp ${returnLabel}`)
          return
        }
        case 'syscall': {
          assert(node.code.kind == 'numberLiteral')
          const nr = node.code.n
          assert(nr !== undefined, 'must be a linux syscall')
          // TODO: code->name mapping for bebug info
          lines.push(`; ${node.code.n} syscall`)
          const argRegisters = ['rdi', 'rsi', 'rdx', 'r10', 'r8', 'r9']
          for (let i = 0; i < node.args.length; i++) {
            const arg = node.args[i]
            assert(typeof arg === 'object', 'must be object')
            emitExpr(arg)
            lines.push(`pop ${argRegisters[i]}`)
            // lines.push(`mov ${argRegisters[i]}, ${emitVar(arg.varDec)}`);
          }
          lines.push(`mov rax, ${nr}`)
          lines.push('syscall')
          if (shouldReturn) lines.push('push rax')
          lines.push('')
          return
        }
        case 'call': {
          lines.push(
            `; ${node.def.name}(${node.def.params.map(n => n.name).join(', ')})`
          )
          const args = node.args
          const argSize = args.length * 8
          if (argSize) {
            lines.push(`sub rsp, ${argSize}`)
            let off = argSize
            for (let arg of args) {
              assert(arg.kind === 'numberLiteral')
              off -= 8
              lines.push(`mov qword [rsp+${off}], ${arg.n}`)
              console.log(`arg [rsp+${off}] = ${arg.n}`)
            }
          }
          lines.push(`call _${node.def.name}`)
          if (argSize) {
            lines.push(`add rsp, ${argSize}`)
          }
          if (shouldReturn) lines.push(`push rax`)
          lines.push(``)
          return
        }
        case 'if': {
          const then = label()

          lines.push('; if ()')
          emitExpr(node.cond)
          lines.push(`pop rax`)
          lines.push(`cmp rax, 1`)

          lines.push(`jne .${then}`)
          lines.push('; then')
          assert(Array.isArray(node.then), 'if.then is array')
          for (let e of node.then) emitExpr(e, { shouldReturn: false })

          if (node.els) {
            assert(Array.isArray(node.els, 'if.els is array'))
            const end = label()
            lines.push(`jmp .${end}`)
            lines.push('; else')
            lines.push(`.${then}:`)
            for (let e of node.els) emitExpr(e, { shouldReturn: false })
            lines.push(`.${end}:`)
          } else {
            lines.push(`.${then}:`)
          }
          lines.push('; endif\n')

          return
        }
        case 'readVar': {
          assert(shouldReturn, 'readVar should not be called at top level')
          lines.push(`push qword ${emitVar(node.varDec)}`)
          return
        }
        case 'readProp': {
          assert(node.varDec.notes?.has('const'), 'should be a constant')
          assert(
            node.varDec.expr.kind == 'stringLiteral',
            'should be initalized to a string literal'
          )
          assert(node.prop == 'length', 'property should be length')

          lines.push(`push ${node.varDec.expr.value.length}`)

          return
        }
        case 'binary': {
          const ops = {
            '+': 'add',
            '-': 'sub',
            '*': 'imul',
            '/': 'idiv',
            '%': 'idiv',

            '>': 'setg',
            '>=': 'setge',
            '<': 'setl',
            '<=': 'setle',
            '==': 'sete',
            '!=': 'setne'
          }
          const op = ops[node.op]
          assert(op, `illegal binary operator ${node.op}`)

          let resultRegister = node.op == '%' ? 'rdx' : 'rax'

          emitExpr(node.a)
          emitExpr(node.b)
          lines.push(`pop rcx`)
          lines.push(`pop rax`)
          if (op.startsWith('set')) {
            lines.push(`cmp rax, rcx`)
            // TODO: figure out if this is safe?
            // we save the result in al but push rax
            // so we're possibly saving garbage data from the upper bytes
            lines.push(`${op} al`)
          } else if (op == 'idiv') {
            // as seen in C -> asm view on godbolt
            // cqo sign-extends rax:rdx
            // not really sure what this means, but it's needed to make *signed* division work
            // for unsigned division we'd use 'mul' over 'imul' and ditch 'cqo'
            // https://en.wikipedia.org/wiki/Sign_extension
            // https://stackoverflow.com/questions/36464879/when-and-why-do-we-sign-extend-and-use-cdq-with-mul-div
            // https://stackoverflow.com/questions/51717317/dividing-with-a-negative-number-gives-me-an-overflow-in-nasm/51717463#51717463
            lines.push(`cqo`)
            lines.push(`${op} rcx`)
          } else {
            lines.push(`${op} rax, rcx`)
          }
          if (shouldReturn) lines.push(`push ${resultRegister}`)

          return
        }
        case 'unary': {
          const ops = {
            'pre++': 'inc',
            'post++': 'inc',
            'pre--': 'dec',
            'post--': 'dec',
            '!': 'not',
            '-': 'neg'
          }
          assert(node.a.kind == 'readVar')
          const op = ops[node.op]
          assert(op !== undefined, `illegal unary operator ${node.op}`)

          if (shouldReturn && node.op.startsWith('pre')) emitExpr(node.a)
          lines.push(`${op} qword ${emitVar(node.a.varDec)}`)
          if (shouldReturn && node.op.startsWith('post')) emitExpr(node.a)
          return
        }
        case 'numberLiteral': {
          assert(shouldReturn, 'number should not be called at top level')
          // TODO: get a fitting empty register instead of hardcoded rax
          lines.push(`push ${node.n}`)
          return
        }
        case 'stringLiteral': {
          assert(
            shouldReturn,
            'string literal should not be called at top level'
          )
          lines.push(`push ${label(node.value)}`)
          return
        }
        case 'assignVar': {
          console.assert(
            typeof node.varDec === 'object',
            'vardef must be object'
          )

          emitExpr(node.expr)
          lines.push(`; ${node.varDec.name} = expr`)
          lines.push(`pop rax`)
          lines.push(`mov ${emitVar(node.varDec)}, rax\n`)
          if (shouldReturn) lines.push(`push rax`)
          return
        }
        default:
          console.log(node)
          assert(false, `unsupported node kind ${node.kind}`)
      }
    }
    for (let node of ast) emitTop(node)
    assert(hasMain, 'the program defines a main function')
    const source = `
section .text
global _start
${lines.join('\n')}
section .data
${[...data.keys()]
  .map(k => {
    let msg = data.get(k)
    if (typeof msg === 'number') {
      return `${k} equ ${msg}`
    }

    assert(typeof msg === 'string', 'only string data is supported')

    const bytes = [...msg].map(c => c.charCodeAt(0)).join(',')
    return `${k} db ${bytes} ; '${msg.replace(/\n/g, '\\n')}'`
  })
  .join('\n')}
`
    write(dest, source)
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
  statx: 0x14c
}

module.exports = api
