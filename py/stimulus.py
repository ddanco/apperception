import random

from psychopy import core, event, visual

# https://www.psychopy.org/coder/tutorial1.html

window = visual.Window([800,600], monitor="testMonitor", units="deg")

random.seed()
rand_x = lambda: random.randint(-10, 10)
rand_y = lambda: random.randint(-5, 5)

polys = []

def draw_if_possible(item):
    if all (not item.overlaps(poly) for poly in polys):
        item.draw()
        polys.append(item)

for color in ["red", "blue"]:
    for i in range(random.randint(2, 4)):
        circ = visual.Circle(
            win=window,
            pos=[rand_x(), rand_y()],
            radius=0.5,
            contrast=0.7,
            fillColor=color)
        draw_if_possible(circ)
    for j in range(random.randint(2, 4)):
        rect = visual.Rect(
            win=window,
            pos=[rand_x(), rand_y()],
            size=1,
            contrast=0.7,
            fillColor=color)
        draw_if_possible(rect)

window.update()
core.wait(2.0)
