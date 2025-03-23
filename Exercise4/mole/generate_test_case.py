import sys
import random

def generate_worst_case():
    n = 20
    d = 5
    m = 1000
    max_time = 10
    moles_per_time = m // max_time  # 100 moles per time step

    # Generate all possible grid coordinates (0 ≤ x, y < 20)
    all_coords = [(x, y) for x in range(n) for y in range(n)]
    
    # Seed for reproducibility (optional)
    # random.seed(42)

    # Generate moles for each time step
    moles = []
    for t in range(1, max_time + 1):
        # Shuffle coordinates to ensure uniqueness within this time step
        random.shuffle(all_coords)
        selected = all_coords[:moles_per_time]
        for x, y in selected:
            moles.append((x, y, t))

    # Output the test case
    print(f"{n} {d} {m}")
    for x, y, t in moles:
        print(f"{x} {y} {t}")
    # End of input marker
    print("0 0 0")

if __name__ == "__main__":
    generate_worst_case()