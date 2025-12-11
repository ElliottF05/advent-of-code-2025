import time
from scipy.optimize import milp, LinearConstraint, Bounds
import numpy as np

def parse_joltage_or_button(str):
    str = str.strip()[1:-1]
    return [int(sub) for sub in str.split(',')]

def parse_line(str):
    parts = str.split(' ')

    joltage_str = parts[-1]
    joltage = parse_joltage_or_button(joltage_str)

    button_strs = parts[1:-1]
    buttons = [parse_joltage_or_button(s) for s in button_strs]

    return joltage, buttons

def solve(joltages, buttons):
    c = np.array([1] * len(buttons))
    integrality = np.array([1] * len(buttons))

    upper = np.array([max(joltages) * len(buttons)])
    lower = np.array([0] * len(buttons))
    bounds = Bounds(lower, upper)

    b = np.array(joltages)
    A = np.zeros((b.shape[0], c.shape[0]))
    for i,button in enumerate(buttons):
        for j in button:
            A[j,i] = 1
    
    constraint = LinearConstraint(A, b, b)

    res = milp(
        c=c,
        integrality=integrality,
        bounds=bounds,
        constraints=[constraint],
    )

    return res.fun


def main():
    start = time.time()
    res = 0
    with open("part1.txt", 'r') as file:
        for line in file:
            joltages, buttons = parse_line(line)
            res += solve(joltages, buttons)
    end = time.time()
    elapsed = end - start

    print(f"Answer: {int(res)} (time taken: {elapsed})")
    
if __name__ == "__main__":
    main()