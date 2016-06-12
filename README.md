# SimpeTableGenerator

## About

This library is for drawing text tables from 2d-lists.

Pass a list of lists of strings and get a single string with table contents.

```
makeDefaultSimpleTable :: [[String]] -> String
```

## Basic usage

```
putStrLn $ makeDefaultSimpleTable [["1","2","3"], ["One","Two","Three"], ["First", "Second"]]
```
```
┌───────┬────────┬───────┐
│ 1     │ 2      │ 3     │
├───────┼────────┼───────┤
│ One   │ Two    │ Three │
├───────┼────────┼───────┤
│ First │ Second │       │
└───────┴────────┴───────┘
```

## Advanced usage

You can configure the table by constructing `SimpleTableConfig` and passing it to `makeSimpleTable`.

```
putStrLn $ makeSimpleTable simpleTableConfig {
    tableBorders = "+++++++++-|",
    colMinWidths  = [3, 4],
    rowMinHeights = [2],
    padFunction   = simpleTableLeftPad,
    cellPadFunction = simpleTableBottomPad,
    horizontalPadding = 0,
    verticalPadding = 1,
    paddingStr = ".,`"
    } [["a"],["b","c"]]
```
```
+---+----+
|.,`|.,`.|
|.,a|.,`.|
|.,`|.,`.|
|.,`|.,`.|
+---+----+
|.,`|.,`.|
|.,b|.,`c|
|.,`|.,`.|
+---+----+
```

### Custom borders
Only single-character borders are currently supported.

Default `tableBorders` are  `"┌┬┐├┼┤└┴┘─│"`
### Limiting cell sizes
You can set minimum cell sizes for each colon and row by constructing new `SimpleTableConfig` with appropriative `colMinWidths` and `rowMinHeights`.

You are not able to set maximum cell sizes because they are calculating from passed list-of-lists-of-strings dynamically (and in any case text in each cell remains unchanged). If you still need to do this, you may manually wrap or cut long lines in your table before passing it to the library functions.

### Padding cells
There are two ways of customizing padding style:
1. By changing `horizontalPadding` and/or `verticalPadding` of `SimpleTableConfig`
2. By passing your own padding functions as `padFunction` and/or `cellPadFunction`

   These functions must have the following type signatures:
#### Horizontal padding
```
padFunction :: String -> Int -> String -> String
```
#### Vertical padding
```
cellPadFunction :: String -> Int -> [String] -> [String]
```
