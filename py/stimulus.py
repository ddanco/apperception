import random

from psychopy import core, event, visual

# https://www.psychopy.org/coder/tutorial1.html

######################
#      CONFIGS       #
######################

RANDOM = False
COLORS = ["red", "blue"]

######################

window = visual.Window([800,600], monitor="testMonitor", units="deg")

random.seed()
rand_x = lambda: random.randint(-10, 10)
rand_y = lambda: random.randint(-5, 5)

polys = []

def add_if_possible(item):
    if all (not item.overlaps(poly) for poly in polys):
        item.draw()
        polys.append(item)

for color in COLORS:
    for i in range(random.randint(2, 4)):
        circ = visual.Circle(
            win=window,
            pos=[rand_x(), rand_y()],
            radius=0.5,
            contrast=0.2,
            fillColor=color)
        add_if_possible(circ)
    for j in range(random.randint(2, 4)):
        rect = visual.Rect(
            win=window,
            pos=[rand_x(), rand_y()],
            size=1,
            contrast=0.2,
            fillColor=color)
        add_if_possible(rect)

for item in polys:
    item.draw()
window.update()
core.wait(0.5)

if RANDOM:
    ## Random order attention
    random.shuffle(polys)
else:
    ## Left-to-right attention
    polys.sort(key = lambda S: S.pos[0])

for i, _ in enumerate(polys):
    for item in polys:
        item.draw()
    for k in range(i+1):
        overlay = polys[k]
        overlay.setContrast(0.7)
        overlay.draw()
    window.update()
    core.wait(0.3)

core.wait(2.0)
