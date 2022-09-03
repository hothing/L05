# BUGS list

## Reductor

Код модифицируется корретно, но не реализована модификация списка переменных

## Graph coloring with DSATUR

Input>
[V5.2] (map
   [(X0RV "t.1", set [X0RV "t.2"; X0RV "z"]); 
    (X0RV "t.2", set [X0RV "t.1"]);
    (X0RV "v", set [X0RV "w"]);
    (X0RV "w", set [X0RV "v"; X0RV "x"; X0RV "y"; X0RV "z"]);
    (X0RV "x", set [X0RV "w"; X0RV "y"]);
    (X0RV "y", set [X0RV "w"; X0RV "x"; X0RV "z"]);
    (X0RV "z", set [X0RV "t.1"; X0RV "w"; X0RV "y"]); (X0RNone, set [])],
 map
   [(X0RV "t.1", 0); (X0RV "t.2", 0); (X0RV "v", 0); (X0RV "w", 0);
    (X0RV "x", 0); (X0RV "y", 0); (X0RV "z", 0); (X0RNone, 0)],
 set [1; 2; 3; 4; 5; 6; 7; 8; 9])

Output>
[DBG][doColorization] sudoku map
  [(X0RV "t.1", (0, 1)); (X0RV "t.2", (0, 1)); (X0RV "v", (0, 1));
   (X0RV "w", (0, 1)); (X0RV "x", (0, 1)); (X0RV "y", (0, 1));
   (X0RV "z", (0, 1)); (X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 0
[DBG][doColorization] selected vertice X0RV "t.1"
[DBG][doColorization] selected color 1
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 0); (X0RV "v", 0); (X0RV "w", 0); (X0RV "x", 0);
   (X0RV "y", 0); (X0RV "z", 0); (X0RNone, 0)]
[DBG][doColorization] sudoku map
  [(X0RV "t.2", (1, 2)); (X0RV "v", (0, 1)); (X0RV "w", (0, 1));
   (X0RV "x", (0, 1)); (X0RV "y", (0, 1)); (X0RV "z", (1, 2)); (X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 1
[DBG][doColorization] selected vertice X0RV "t.2"
[DBG][doColorization] selected color 2
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 0); (X0RV "w", 0); (X0RV "x", 0);
   (X0RV "y", 0); (X0RV "z", 0); (X0RNone, 0)]
[DBG][doColorization] sudoku map
  [(X0RV "v", (0, 1)); (X0RV "w", (0, 1)); (X0RV "x", (0, 1));
   (X0RV "y", (0, 1)); (X0RV "z", (1, 2)); (X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 1
[DBG][doColorization] selected vertice X0RV "z"
[DBG][doColorization] selected color 2
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 0); (X0RV "w", 0); (X0RV "x", 0);
   (X0RV "y", 0); (X0RV "z", 2); (X0RNone, 0)]
[DBG][doColorization] sudoku map
  [(X0RV "v", (0, 1)); (X0RV "w", (1, 1)); (X0RV "x", (0, 1));
   (X0RV "y", (1, 1)); (X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 1
[DBG][doColorization] selected vertice X0RV "w"
[DBG][doColorization] selected color 1
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 0); (X0RV "w", 1); (X0RV "x", 0);
   (X0RV "y", 0); (X0RV "z", 2); (X0RNone, 0)]
[DBG][doColorization] sudoku map
  [(X0RV "v", (1, 2)); (X0RV "x", (1, 2)); (X0RV "y", (2, 3)); (X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 2
[DBG][doColorization] selected vertice X0RV "y"
[DBG][doColorization] selected color 3
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 0); (X0RV "w", 1); (X0RV "x", 0);
   (X0RV "y", 3); (X0RV "z", 2); (X0RNone, 0)]
[DBG][doColorization] sudoku map [(X0RV "v", (1, 2)); (X0RV "x", (2, 2)); (X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 2
[DBG][doColorization] selected vertice X0RV "x"
[DBG][doColorization] selected color 2
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 0); (X0RV "w", 1); (X0RV "x", 2);
   (X0RV "y", 3); (X0RV "z", 2); (X0RNone, 0)]
[DBG][doColorization] sudoku map [(X0RV "v", (1, 2)); (X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 1
[DBG][doColorization] selected vertice X0RV "v"
[DBG][doColorization] selected color 2
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 2); (X0RV "w", 1); (X0RV "x", 2);
   (X0RV "y", 3); (X0RV "z", 2); (X0RNone, 0)]
[DBG][doColorization] sudoku map [(X0RNone, (0, 1))]
[DBG][doColorization] highest saturation level = 0
[DBG][doColorization] selected vertice X0RNone
[DBG][doColorization] selected color 1
[DBG][doColorization] new color map map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 2); (X0RV "w", 1); (X0RV "x", 2);
   (X0RV "y", 3); (X0RV "z", 2); (X0RNone, 1)]
[DBG][doColorization] finish
[V5.3] map
  [(X0RV "t.1", 1); (X0RV "t.2", 2); (X0RV "v", 2); (X0RV "w", 1); (X0RV "x", 2);
   (X0RV "y", 3); (X0RV "z", 2); (X0RNone, 1)]

