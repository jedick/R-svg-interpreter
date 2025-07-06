The SVG isn't that complex, it defines two colour gradients (one for the R and one for the ring) and then draws each one using a series of cubic bezier curves and lines.

These are in <path> tags in the SVG which look like this: M361.453,485.937 C162.329,485.937 0.906,377.828 0.906,244.469C0.906,111....etc.

M is "move to" and then "C" defines a curve, "L" defines a straight line. I don't think any other path items are used.

There's two paths, each being two parts because both the R and the ring have holes in them.

If you want to draw this from scratch in R code, first write (or find) code to draw cubic bezier curves as per the SVG spec, then extract the coordinates from the path attributes in the SVG then make polygons and shade them using the colour gradients at the start of the SVG...
