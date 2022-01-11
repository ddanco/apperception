import random

from psychopy import core, event, visual

# https://www.psychopy.org/coder/tutorial1.html

window = visual.Window([800,600], monitor="testMonitor", units="deg")

random.seed()
rand_x = lambda: random.randint(-10, 10)
rand_y = lambda: random.randint(-5, 5)

for color in ["red", "blue"]:
    for i in range(random.randint(2, 4)):
        visual.Circle(
            win=window,
            pos=[rand_x(), rand_y()],
            radius=0.5,
            fillColor=color).draw()
    for j in range(random.randint(2, 4)):
        visual.Rect(
            win=window,
            pos=[rand_x(), rand_y()],
            size=1,
            fillColor=color).draw()

window.update()
core.wait(2.0)
