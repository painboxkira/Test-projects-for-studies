import tkinter as tk
from tkinter import messagebox, ttk

# Function to calculate the total bill
def calculate_bill():
    try:
        # Initialize subtotal
        subtotal = 0
        
        # Check which services are selected and add to the subtotal
        if parallellism_var.get():
            subtotal += 60
        if radiator_var.get():
            subtotal += 80
        if headlights_var.get():
            subtotal += 20
        
        # Add crevaison repairs
        crevaison_count = int(crevaison_entry.get() or 0)
        subtotal += crevaison_count * 15

        # Add wheel balancing
        balancing_count = int(balancing_entry.get() or 0)
        subtotal += balancing_count * 10

        # Add pont repair
        pont_price = float(pont_entry.get() or 0)
        subtotal += pont_price

        # Add tire purchases
        tire_count = int(tire_count_entry.get() or 0)
        tire_price = float(tire_price_entry.get() or 0)
        subtotal += tire_count * tire_price

        # Calculate fees and taxes
        service_fee = subtotal * 0.05
        tax = subtotal * 0.10
        total = subtotal + service_fee + tax

        # Display the final bill in a fancy table
        display_bill(subtotal, service_fee, tax, total)

    except ValueError:
        messagebox.showerror("Invalid Input", "Please enter valid numbers for the inputs.")

# Function to display the bill
def display_bill(subtotal, service_fee, tax, total):
    bill_window = tk.Toplevel(root)
    bill_window.title("Final Bill")

    # Create a treeview for the bill table
    columns = ("Description", "Amount (DH)")
    bill_table = ttk.Treeview(bill_window, columns=columns, show="headings")
    bill_table.heading("Description", text="Description")
    bill_table.heading("Amount (DH)", text="Amount (DH)")

    # Add rows to the table
    bill_table.insert("", "end", values=("Subtotal", f"{subtotal:.2f}"))
    bill_table.insert("", "end", values=("Service Fee (5%)", f"{service_fee:.2f}"))
    bill_table.insert("", "end", values=("Tax (10%)", f"{tax:.2f}"))
    bill_table.insert("", "end", values=("Total", f"{total:.2f}"))

    bill_table.pack(padx=10, pady=10)

    # Add a print or save button
    print_button = tk.Button(bill_window, text="Print/Save Bill", command=lambda: save_bill(subtotal, service_fee, tax, total))
    print_button.pack(pady=10)

# Function to save the bill
def save_bill(subtotal, service_fee, tax, total):
    with open("garage_bill.txt", "w") as file:
        file.write("---- Garage Final Bill ----\n")
        file.write(f"Subtotal: {subtotal:.2f} DH\n")
        file.write(f"Service Fee (5%): {service_fee:.2f} DH\n")
        file.write(f"Tax (10%): {tax:.2f} DH\n")
        file.write(f"Total: {total:.2f} DH\n")
    messagebox.showinfo("Saved", "The bill has been saved to 'garage_bill.txt'.")

# Main GUI
root = tk.Tk()
root.title("Garage Services Bill")

# Service selection checkboxes
parallellism_var = tk.BooleanVar()
radiator_var = tk.BooleanVar()
headlights_var = tk.BooleanVar()

tk.Checkbutton(root, text="Parallélisme des roues (60 DH)", variable=parallellism_var).grid(row=0, column=0, sticky="w")
tk.Checkbutton(root, text="Nettoyage radiateur (80 DH)", variable=radiator_var).grid(row=1, column=0, sticky="w")
tk.Checkbutton(root, text="Réglage des phares (20 DH)", variable=headlights_var).grid(row=2, column=0, sticky="w")

# Crevaison repairs
tk.Label(root, text="Nombre de crevaisons réparées (15 DH/unité):").grid(row=3, column=0, sticky="w")
crevaison_entry = tk.Entry(root)
crevaison_entry.grid(row=3, column=1)

# Wheel balancing
tk.Label(root, text="Nombre d'équilibrages (10 DH/unité):").grid(row=4, column=0, sticky="w")
balancing_entry = tk.Entry(root)
balancing_entry.grid(row=4, column=1)

# Pont repair
tk.Label(root, text="Prix de la réparation du pont avant (variable):").grid(row=5, column=0, sticky="w")
pont_entry = tk.Entry(root)
pont_entry.grid(row=5, column=1)

# Tire purchases
tk.Label(root, text="Nombre de pneus achetés:").grid(row=6, column=0, sticky="w")
tire_count_entry = tk.Entry(root)
tire_count_entry.grid(row=6, column=1)

tk.Label(root, text="Prix unitaire d'un pneu:").grid(row=7, column=0, sticky="w")
tire_price_entry = tk.Entry(root)
tire_price_entry.grid(row=7, column=1)

# Calculate button
calculate_button = tk.Button(root, text="Calculate Bill", command=calculate_bill)
calculate_button.grid(row=8, column=0, columnspan=2, pady=10)

root.mainloop()