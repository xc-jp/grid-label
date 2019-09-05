# grid-label
Very simple annotation tool for segmentation networks. Usage:
```
grid-label FILENAME+ (-w|--width ARG) (-h|--height ARG)
```

Example:
- `grid-label image.jpg -w 20 -h 10` will create a single 20 by 10 monochrome image `image.label.bmp` for your label
- `grid-label dir/*.jpg -w 20 -h 10` allows you to annotate large numbers of images at once, provided they are all of the same size.

Controls:
- Clicking the grid marks a grid-point
- Escape exits and saves labels
- left/right will move one image backward/forward

