import argparse
import logging
import os
import select
import socket
import sys
import multiprocessing

from F2x.parser.plyplus.source import load_grammar
from F2x.runtime.main import main as f2x_main
from F2x.runtime.argp import init_logger

_log = logging.getLogger(__name__)


class SocketStream(socket.SocketIO):
    def write(self, data):
        return super().write(data.encode('utf-8'))


def _log_args():
    args = argparse.Namespace()
    args.verbose = 5
    args.quiet = 0
    args.logfile = None

    return args


def _preload_grammar():
    global _log
    _log = init_logger(_log_args(), fmt=f'%(levelname)-5s [{multiprocessing.current_process().name}] %(message)s')

    load_grammar('@fortran.g')


def _run_process(pwd, args, conn):
    handler = logging.StreamHandler(SocketStream(conn, 'w'))
    handler.setFormatter(logging.Formatter(f'%(levelno)d:{multiprocessing.current_process().name}:%(message)s\n'))

    log = logging.getLogger()
    log.addHandler(handler)

    os.chdir(pwd)
    f2x_main(args)

    log.removeHandler(handler)


class F2xDaemon(object):
    def __init__(self, addr, port, num_procs):
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._socket.bind((addr, port))
        self._socket.listen(num_procs)
        self._pool = multiprocessing.Pool(num_procs, initializer=_preload_grammar)

    def serve(self):
        while True:
            try:
                conn, addr = self._socket.accept()
                _log.info("New request from %s.", addr)

            except KeyboardInterrupt:
                _log.info("Stopping...")
                self._socket.close()
                self._pool.close()
                self._pool.join()
                break

            pwd, *args = conn.recv(4096).decode('utf-8').splitlines()
            self._pool.apply_async(_run_process, (pwd, args, conn), callback=lambda _: conn.close(), error_callback=lambda _: conn.close())


class F2xClient(object):
    def __init__(self, host, port):
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._socket.connect((host, port))
        self._socket.setblocking(False)

    def invoke(self, args):
        pwd = os.getcwd()
        data = "\n".join([pwd] + args)
        self._socket.send(data.encode('utf-8'))
        failed = 0

        inputs = [self._socket]

        while inputs:
            readable, _, exceptional = select.select(inputs, [], inputs)

            for s in readable:
                data = s.recv(4096)
                if data:
                    _log.info(data.decode('utf-8'))
                else:
                    inputs.remove(s)
                    s.close()

            for s in exceptional:
                inputs.remove(s)
                s.close()
                failed += 1

        return failed


def main():
    argp = argparse.ArgumentParser(description="F2x daemon")
    argp.add_argument('-l', '--listen', nargs=2, metavar=('HOST', 'PORT'), required=False)
    argp.add_argument('-n', '--num-procs', nargs=1, action='store', type=int, default=[1])
    argp.add_argument('-c', '--connect', nargs=2, metavar=('HOST', 'PORT'), required=False, default=('localhost', 2337))

    daemon_args, args = argp.parse_known_args()

    global _log
    _log = init_logger(_log_args())

    if daemon_args.listen:
        host, port = daemon_args.listen
        daemon = F2xDaemon(host, int(port), daemon_args.num_procs[0])
        daemon.serve()

    else:
        host, port = daemon_args.connect
        daemon = F2xClient(host, int(port))
        sys.exit(daemon.invoke(args))


if __name__ == '__main__':
    main()
