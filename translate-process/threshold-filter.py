#!/usr/bin/env python

import sys

for line in sys.stdin:
    try:
        key_value = int(line)

        if key_value < 0:
            print("?blue4?{}".format(key_value))
        elif key_value < 4:
            print(key_value)
        else:
            print("?brown4?{}".format(key_value))

    except ValueError:
        print("?dark red?NaN")

    sys.stdout.flush()
