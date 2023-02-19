import importlib
import sys


def main(args):
    if len(args) != 3:
        sys.stderr.write("usage: python oracle.py PROTO_MODULE MESSAGE\n")
        return 1
    mod = importlib.import_module(f"oracle.{args[1]}_pb2")
    msg = getattr(mod, args[2])
    val = msg()
    val.ParseFromString(sys.stdin.buffer.read())
    sys.stderr.write(str(val))
    sys.stdout.buffer.write(val.SerializeToString())
    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
