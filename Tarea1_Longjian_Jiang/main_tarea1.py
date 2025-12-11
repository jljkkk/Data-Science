

import tkinter as tk
from tkinter import messagebox
import juego

window = tk.Tk()
window.title("Guess the Number Game")
window.geometry("1000x500")

screen = tk.Frame(window)
message = tk.Label(screen, text="Welcome to the Guess the Number Game!", font=("Arial", 24))
message.pack(pady=20)
screen.pack(fill ="both", expand = True)

botons = tk.Frame(window)
boton1 = tk.Button(botons, text="1.Solo mode", font=("Arial", 18),command = lambda:juego.solo_mode(window))
boton1.grid(column=0, row=0, padx=20, pady=20)

boton2 = tk.Button(botons, text="2.Two players mode", font=("Arial", 18), command=lambda:juego.two_players_mode(window))
boton2.grid(column=0, row=1, padx=20, pady=20)

boton3 = tk.Button(botons, text="3.Check statistics", font=("Arial", 18), command=lambda:juego.check_statistics())
boton3.grid(column=0, row=2, padx=20, pady=20)

boton4 = tk.Button(botons, text="4.Exit", font=("Arial", 18), command=window.quit)
boton4.grid(column=0, row=3, padx=20, pady=20)

botons.pack()

window.mainloop()