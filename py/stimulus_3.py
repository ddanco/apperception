import random

from psychopy import core, event, visual

# https://www.psychopy.org/coder/tutorial1.html

######################
#      CONFIGS       #
######################

ATTN        = False
ITERATIONS  = 10

######################

window = visual.Window([800,600], monitor="testMonitor", units="deg")

start_contrast = 0.2
if not ATTN:
    start_contrast = 0.7

c1 = visual.Circle(
  win=window,
  pos=[-5, 3],
  radius=1,
  contrast=start_contrast,
  fillColor="blue")

c2 = visual.Circle(
  win=window,
  pos=[-5, -3],
  radius=1,
  contrast=start_contrast,
  fillColor="purple")

r1 = visual.Rect(
  win=window,
  pos=[5, 3],
  size=2,
  contrast=start_contrast,
  fillColor="red")

r2 = visual.Rect(
  win=window,
  pos=[5, -3],
  size=2,
  contrast=start_contrast,
  fillColor="orange")

def draw(items):
  for item in items:
    item.draw()
  window.update()
  core.wait(0.5)

if not ATTN:
  for i in range(ITERATIONS):
    draw([c1, r1])
    draw([c2, r2])

else:
  # FIXME: This is a MESS
  random.seed()
  polys = [c1, c2, r1, r2]
  draw(polys)
  top_left = random.randint(0,1)
  top_right = random.randint(0,1)
  first_loop = True
  for i in range(ITERATIONS):
    if top_left:
      top_1 = c1
      top_2 = r1
    else:
      top_1 = r1
      top_2 = c1
    if top_right:
      bottom_1 = c2
      bottom_2 = r2
      top_stop_1 = c1
      top_stop_2 = r1
    else:
      bottom_1 = r2
      bottom_2 = c2
      top_stop_1 = r1
      top_stop_2 = c1

    if not first_loop:
      if top_left:
        c2.setContrast(0.3)
      else:
        r2.setContrast(0.3)
    top_1.setContrast(0.7)
    draw(polys)
    if not first_loop:
      if top_left:
        r2.setContrast(0.3)
      else:
        c2.setContrast(0.3)
    top_1.setContrast(0.7)
    top_2.setContrast(0.7)
    draw(polys)
    top_stop_1.setContrast(0.3)
    bottom_1.setContrast(0.7)
    draw(polys)
    top_stop_2.setContrast(0.3)
    bottom_2.setContrast(0.7)
    draw(polys)

    top_left = random.randint(0,1)
    top_right = random.randint(0,1)

    first_loop = False

core.wait(1.0)
