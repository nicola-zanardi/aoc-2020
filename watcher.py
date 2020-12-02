#!/bin/python

import argparse
import os
from time import sleep

# simple thing to run on save


def main(filename, action):
    last_time = None
    while True:
        current_time = os.stat(filename).st_mtime
        if current_time != last_time:
            os.system("cls")
            print(f"{os.path.basename(filename)} changed...")
            last_time = current_time
            os.system(action)
        sleep(1)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--file", nargs="?", help="file to watch")
    parser.add_argument("--action", nargs="?", help="Action to perform on change")
    args = parser.parse_args()
    main(args.file, args.action)
