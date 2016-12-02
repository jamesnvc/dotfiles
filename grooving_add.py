#!/home/james/.pyenv/shims/python3

import tkinter as tk
import grooving


class Application(tk.Frame):

    def __init__(self, master=None):
        super().__init__(master)
        self.title = "Add Dialog"
        self.pack()

        self.create_widgets()

    def create_widgets(self):

        self.bind('<Destroy>', self.on_exit)
        self.master.bind('<Key-Escape>', self.exit)

        self.exercise_entry = tk.Entry()
        self.exercise_entry.pack()
        self.exercise = tk.StringVar()
        self.exercise.set("")
        self.exercise_entry["textvariable"] = self.exercise
        self.exercise_entry.bind('<Key-Return>', self.maybe_save)

        self.count_entry = tk.Entry()
        self.count_entry.pack()
        self.count = tk.IntVar()
        self.count.set(0)
        self.count_entry["textvariable"] = self.count
        self.count_entry.bind('<Key-Return>', self.maybe_save)

    def on_exit(self, event):
        grooving.todays_exercises()

    def exit(self, event):
        self.master.destroy()

    def maybe_save(self, event):
        exercise = self.exercise.get()
        count = self.count.get()
        grooving.increment_count(exercise, count)
        self.exit()


def main():
    root = tk.Tk()
    root.title("Add Dialog")
    app = Application(master=root)
    app.mainloop()

if __name__ == '__main__':
    main()
