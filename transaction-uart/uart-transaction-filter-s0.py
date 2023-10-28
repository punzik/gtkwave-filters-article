#!/usr/bin/env python

import sys

# Версия print c принудительным сбросом буфера stdout
def pprint(*args, **kwargs):
    print(*args, **kwargs)
    sys.stdout.flush()

for line in sys.stdin:
    # Выводим дамп в stderr, чтобы посмотреть, что нам прислал GTKWave
    sys.stderr.write(line)

    # По окончанию приёма блока пошлём ответ
    if (line.startswith("$comment data_end")):
        pprint("$name New Signal")
        pprint("#0 ?dark cyan?Just Text")
        pprint("$finish")
