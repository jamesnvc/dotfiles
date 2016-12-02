#!/home/james/.pyenv/shims/python3

import datetime
import os
import sqlite3
import grooving_add


def get_connection():
    return sqlite3.connect(os.path.expanduser("~/grooving.sqlite"),
                           detect_types=sqlite3.PARSE_DECLTYPES)


def init_db():
    conn = get_connection()
    c = conn.cursor()
    c.execute("""CREATE TABLE IF NOT EXISTS exercises (
        date DATE,
        exercise TEXT,
        count INTEGER
    )""")
    conn.commit()
    conn.close()


def increment_count(exercise, count):
    today = datetime.date.today()
    conn = get_connection()
    c = conn.cursor()
    c.execute("SELECT count FROM exercises WHERE date = ? AND exercise = ?",
              (today, exercise))
    cur = c.fetchone()
    if cur is None:
        c.execute("""INSERT INTO exercises (date, exercise, count)
                  VALUES (?, ?, ?)""", (today, exercise, count))
    else:
        c.execute("""UPDATE exercises SET count = ?
                  WHERE date = ? AND exercise = ?""",
                  (cur[0] + count, today, exercise))
    conn.commit()
    conn.close()


def current_count(exercise):
    today = datetime.date.today()
    conn = get_connection()
    c = conn.cursor()
    c.execute("SELECT count FROM exercises WHERE date = ? AND exercise = ?",
              (today, exercise))
    count = c.fetchone()
    conn.close()
    if count is None:
        return 0
    else:
        return count


def todays_exercises():
    today = datetime.date.today()
    conn = get_connection()
    c = conn.cursor()
    c.execute("SELECT exercise, count FROM exercises WHERE date = ?",
              (today, ))
    exercises = c.fetchall()
    if len(exercises) == 0:
        print("Nothing so far")
    else:
        print(' '.join("[{0[0]}]: {0[1]}".format(r)
                       for r in exercises))


if __name__ == '__main__':
    if os.getenv("BLOCK_BUTTON"):
        grooving_add.main()
    else:
        init_db()
        todays_exercises()
