#!/usr/bin/env python

import sys

# Версия print c принудительным сбросом буфера stdout
def pprint(*args, **kwargs):
    print(*args, **kwargs)
    print(*args, file = sys.stderr)
    sys.stdout.flush()

sig_name = ""
sig_smpl = []
timestamp = 0

# Возвращает значение сигнала в момент времени
def get_signal(tm):
    s = 0
    for smpl in sig_smpl:
        if smpl[0] <= tm:
            s = smpl[1]
        elif smpl[0] > tm:
            break

    return s

# Возвращает элемент массива sig_smpl, время которого >= tm, а значение равно val
def get_following(tm, val):
    smpl = None
    for n,s in enumerate(sig_smpl):
        if s[0] >= tm and s[1] == val:
            smpl = s
            break

    return smpl

# Главный цикл программы
for line in sys.stdin:
    # Извлекаем имя сигнала с индексом 1
    if line.startswith("$comment seqn 1"):
        sig_name = line.split()[3]
    # Временная метка
    elif line[0] == '#':
        timestamp = int(line[1:])
    # Добавляем сигнал в массив
    elif (line[0] == '0' or line[0] == '1') and line[1] == '1':
        sig_smpl.append((timestamp, int(line[0])))

    # По окончанию приёма блока пошлём ответ
    if line.startswith("$comment data_end"):
        # Найдем минимальное время между переключениями сигнала
        t = sig_smpl[0][0]
        s = sig_smpl[0][1]
        dt = None

        for smpl in sig_smpl:
            if s != smpl[1]:
                dt_new = smpl[0] - t
                dt = dt_new if dt is None else min(dt, dt_new)
                t = smpl[0]
                s = smpl[1]

        pprint("$name {} (flt)".format(sig_name))

        smpl_idx = 0
        t_start = None
        tm = 0

        while True:
            # Ищем стартовый бит
            start = get_following(tm, 0)
            if start is None:
                break

            # Ставим указатель времени на середину стартового бита
            tm = start[0] + dt * 0.5
            b = 0

            # Читаем значение байта
            for n in range(8):
                tm = tm + dt
                b = (b >> 1) | (get_signal(tm) << 7)

            # Ищем стоповый бит
            stop = get_following(tm, 1)
            if stop is None:
                stop = sig_smpl[-1][0]

            # Выводим сигнал
            pprint("#{} {}".format(start[0], chr(b) if b >= 32 and b <=127 else str(b)))
            pprint("#{}".format(stop[0] + dt))

        pprint("$finish")

        # Сбросим массив сигналов для разбора следующего блока
        sig_smpl = []
