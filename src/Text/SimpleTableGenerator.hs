module Text.SimpleTableGenerator (
  makeSimpleTable,
  makeDefaultSimpleTable,
  --
  SimpleTableConfig,
  --
  simpleTableConfig,
  tableBorders,
  colMinWidths,
  rowMinHeights,
  padFunction,
  cellPadFunction,
  horizontalPadding,
  verticalPadding,
  paddingStr,
  --
  simpleTableLeftPad,
  simpleTableCenterPad,
  simpleTableRightPad,
  --
  simpleTableBottomPad,
  simpleTableMiddlePad,
  simpleTableTopPad,
  --
  simpleTableAddVPadding,
  ) where

import Data.List.Split (splitOn)
import Data.List (transpose)

-- What you need to know to understand the code:
-- 1. `Table` is a list of `Row`s
-- 2. `Row` is a list of `Cell`s
-- 3. `Cell` is a list of `CellLine`s
-- 4. Each `CellLine` is a string without line breaks
-- 5. TextTable represents a table that is just a [[String]]
--    (so, these `String`s may contain line breaks)
-- 6. During execution, TextTable converts to Table step by step.

-- types:
type Table = [Row]
type Row   = [Cell]
type Cell  = [CellLine]
type CellLine = String  -- cells can be multiline
type CellSize  = (Int, Int)
type CellSizeTable = [[CellSize]]
type TableSize = (Int, Int)
type TextTable = [[String]]

data CellWrapper =
  CellWrapper {
  cell :: Cell,
  rowNum, colNum, cellWidth, cellHeight :: Int,
  topLeft, top, topRight, right, bottomRight,
  bottom, bottomLeft, left:: String
  } deriving (Show)

-- data type that represents table configuration
data SimpleTableConfig =
  SimpleTableConfig {
  tableBorders :: String,
  colMinWidths :: [Int],
  rowMinHeights :: [Int],
  padFunction :: String -> Int -> String -> String,
  cellPadFunction :: String -> Int -> [String] -> [String],
  horizontalPadding :: Int,
  verticalPadding :: Int,
  paddingStr :: String,
  emptyCellStr :: String
  }

-- default table config
simpleTableConfig =
  SimpleTableConfig {
  tableBorders = "┌┬┐├┼┤└┴┘─│",
  colMinWidths  = [],
  rowMinHeights = [],
  padFunction   = simpleTableRightPad,
  cellPadFunction = simpleTableBottomPad,
  horizontalPadding = 1,
  verticalPadding = 0,
  paddingStr = " ",
  emptyCellStr = ""
  }

makeDefaultSimpleTable table =
  makeSimpleTable simpleTableConfig table

makeSimpleTable config table =
    showTable $
    unwrapTable $
    appendBorders $
    normalizeBorderLengths $
    wrapTable processedConfig $
    padTableCells processedConfig $
    makeCells $
    normalizeColonCount processedConfig $
    makeTextTable table
    where
      processedConfig =
        constructPaddingFunctions $ validateConfig config

-- table processing


-- part 1:
-- convert `Show a => [[a]]` to TextTable (which is [[String]])

makeTextTable :: [[String]] -> TextTable
makeTextTable = makeTextTableWithShow id

makeTextTableWithShow :: Show a => (a -> String) -> [[a]] -> TextTable
makeTextTableWithShow showFunction table =
    map (\x -> map showFunction x) table


-- part 2:
-- put something in empty cells
normalizeColonCount :: SimpleTableConfig -> TextTable -> TextTable
normalizeColonCount config = normalizeColonCountWithStr (emptyCellStr config)

normalizeColonCountWithStr :: String -> TextTable -> TextTable
normalizeColonCountWithStr emptyCellStr textTable =
    map (\row -> addExtraCells row) textTable
    where
        addExtraCells row
            | length row < colonCount = row ++ (take (colonCount - length row)
                $ repeat emptyCellStr)
            | otherwise = row
        colonCount = fst $ getTableSize textTable


getTableSize :: [[a]] -> TableSize
getTableSize = get2DlistSize


-- part 3:
-- convert TextTable to Table by splitting each cell
-- line by line

makeCells :: TextTable -> Table
makeCells = makeCellsWith "\n"

makeCellsWith :: String -> TextTable -> Table
makeCellsWith lineSeparator textTable =
    map (\rowStr  -> map
            (\cellStr -> splitCell cellStr) rowStr) textTable
    where
        splitCell :: String -> Cell
        splitCell cellStr = splitOn lineSeparator cellStr

getCellSizeTable :: Table -> CellSizeTable
getCellSizeTable = map2 getCellSize

get2DlistSize list2d = (maximum $ map length list2d, length list2d)

getCellSize :: Cell -> CellSize
getCellSize = get2DlistSize


-- part 4:
-- pad all contents in Table

padTableCells :: SimpleTableConfig -> Table -> Table
padTableCells config table = (padCellLines . addCellLines) table
  where
    -- adds empty `CellLine`s to each `Cell`
    addCellLines table = zipWith addExtraCellLines table realRowHeights -- (rowHeights (getCellSizeTable table))
    addExtraCellLines row height = map
      (\cell -> (cellPadFunction config) "\n" height cell) row
    -- adds extra spaces to each `CellLine`
    padCellLines table = transpose $
      zipWith padCellList (transpose table) realColWidths
    padCellList  col width = map
      (\cell -> map
               (\celLine ->
                 (padFunction config) (paddingStr config) width celLine)
               cell) col
    -- calculate real colon widths.
    -- limits minimum colon width by values from config
    realColWidths = zipWith max (colWidths cellSizeTable) ((colMinWidths config)++(repeat 0))
    realRowHeights = zipWith max (rowHeights cellSizeTable) ((rowMinHeights config)++(repeat 0))
    cellSizeTable = getCellSizeTable table

-- list of heights for each row
rowHeights :: CellSizeTable -> [Int]
rowHeights sizeTable = map
  (\sth -> maximum $ map snd sth) sizeTable

-- list of widths for each colon
colWidths :: CellSizeTable -> [Int]
colWidths sizeTable  = map
  (\sth -> maximum $ map fst sth) $
  transpose sizeTable


-- part 5:
-- join CellLines

wrapTable config table = wrapCells $ addCellCoords table
  where
    addCellCoords :: Table -> [[(Int, Int, Cell)]]
    addCellCoords table = zipWith
      (\ rowNum list ->
        map (\ (cellNum, cell) ->
              (rowNum, cellNum, cell)) list) [1..] $
      map (\ row -> zip [1..] $
            map (\ cell ->
                  map (\ cellLine -> cellLine) cell) row) table
    wrapCells :: [[(Int, Int, Cell)]] -> [[CellWrapper]]
    wrapCells =
      map2 wrapCell
    wrapCell :: (Int, Int, Cell) -> CellWrapper
    wrapCell (rowNum, colNum, cell) =
      (CellWrapper cell rowNum colNum cellWidth cellHeight
       topLeft top topRight right bottomRight bottom bottomLeft left)
      where
        cellHeight = length cell
        cellWidth  = maximum $ map length cell
        topLeft
          | rowNum == 1 && colNum == 1 = [borders !! 0]          -- ┌
          | colNum == 1 = [borders !! 3]                        -- ├
          | rowNum == 1 = [borders !! 1]                        -- ┬
          | otherwise = [borders !! 4]                          -- ┼
        topRight
          | rowNum == 1 && colNum == width = [borders !! 2]      -- ┐
          | rowNum /= 1 && colNum == width = [borders !! 5]      -- ┤
          | otherwise = ""
        right
          | colNum == width = [borders !! 10]
          | otherwise = ""
        bottomRight
          | rowNum == height && colNum == width = [borders !! 8] -- ┘
          | rowNum == height = [borders !! 7]                   -- ┴
          | colNum == width = [borders !! 5]                    -- ┤
          | otherwise = ""
        bottom
          | rowNum == height = [borders !! 9]                   -- ─
          | otherwise = ""
        bottomLeft
          | rowNum == height && colNum == 1 = [borders !! 6]     -- └
          | rowNum == height = [borders !! 7]                   -- └
          | otherwise  = ""
        top = [borders !! 9]                                    -- ─
        left = [borders !! 10]                                  -- │
    -- table size
    tableSize = getTableSize table
    width = fst tableSize
    height = snd tableSize
    borders =  tableBorders config

normalizeBorderLengths :: [[CellWrapper]] -> [[CellWrapper]]
normalizeBorderLengths =
  map2 normalizeBorderLength
  where
    normalizeBorderLength :: CellWrapper -> CellWrapper
    normalizeBorderLength
      (CellWrapper cell rowNum colNum cellWidth cellHeight topLeft
       top topRight right bottomRight bottom bottomLeft left) =
      (CellWrapper cell rowNum colNum cellWidth cellHeight topLeft
       (takeOf cellWidth top) topRight
       right bottomRight
       (takeOf cellWidth bottom) bottomLeft
       left)
      where
        takeOf n sth =
          take n $ concat $ repeat sth

appendBorders :: [[CellWrapper]] -> [[CellWrapper]]
appendBorders table =
  map2 appendAll table
  where
    appendAll
      (CellWrapper cell rowNum colNum cellWidth cellHeight topLeft
       top topRight right bottomRight bottom bottomLeft left) =
      (CellWrapper
       ((
         -- append bottomLeft
         (\ cell ->
           if rowNum == height then
             (init cell) ++ [bottomLeft ++ last cell]
           else
             cell) .
         -- append bottomRight
         (\ cell ->
          if colNum == width && rowNum == height then
            (init cell) ++ [last cell ++ bottomRight]
            else
          cell) .
         -- append bottom
         (\ cell ->
          if rowNum == height then
            cell ++ [bottom]
            else
            cell) .
         -- append right
         (\ cell ->
          if colNum == width then
             [head cell] ++ (zipWith (++) (tail cell) (repeat right))
           else
             cell) .
         -- append topRight
         (\ cell ->
           (head cell ++ topRight):(tail cell)) .
         -- appendt left
         (\ cell ->
           [head cell] ++ zipWith (++) (repeat left) (tail cell)) .
         -- append topLeft
         (\ cell -> (concat (topLeft : [head cell])):(tail cell)) .
         -- appent top
         (\ cell -> top:cell))
         cell)
       rowNum colNum cellWidth cellHeight topLeft
       top topRight right bottomRight bottom bottomLeft left)
    width  = fst $ getTableSize table
    height = snd $ getTableSize table


-- part 6: unwrap cells

unwrapTable :: [[CellWrapper]] -> [[Cell]]
unwrapTable =
  map2 unwrapCell
  where
    unwrapCell
      CellWrapper {cell = cell} =
      cell


-- part 7: join cells & rows

showTable :: [[[String]]] -> String
showTable textTable = strJoin "\n" $
  map (strJoin "\n") $
  map2 (strJoin "") $
  map transpose textTable
    where
      strJoin :: String -> [String] -> String
      strJoin separator lst = if null lst then
                                ""
                              else
                                foldr1 (\x y -> x ++ separator ++ y) lst

-- horizontal padding functions
simpleTableLeftPad :: String -> Int -> String -> String
simpleTableLeftPad paddingStr width str
  | length str > width = error "SimpleTableGenerator: String's length is greater than maximum!"
  | otherwise = padding ++ str
    where
      padding = take (width - length str) $ concat $ repeat paddingStr

simpleTableRightPad :: String -> Int -> String -> String
simpleTableRightPad paddingStr width str
  | length str > width = error "SimpleTableGenerator: String's length is greater than maximum!"
  | otherwise = str ++ padding
    where
      padding = take (width - length str) $ concat $ repeat paddingStr

simpleTableCenterPad :: String -> Int -> String -> String
simpleTableCenterPad paddingStr width str
  | length str > width = error "SimpleTableGenerator: String's length is greater than maximum!"
  | even (width - length str) = halfPadding ++ str ++ halfPadding
  | otherwise = halfPadding ++ str ++ take (halfWidth + 1) padding
    where
      halfWidth = ((width - length str) `div` 2)
      padding = concat $ repeat paddingStr
      halfPadding = take halfWidth padding

-- vertical padding functions
simpleTableBottomPad :: String -> Int -> Cell -> [String]
simpleTableBottomPad cellStr height cell
  | length cell > height = error "SimpleTableGenerator: Cell's height is greater than maximum!"
  | length cell == height = cell
  | otherwise = cell ++ padding
    where
      padding = replicate (height - length cell) ""

simpleTableTopPad :: String -> Int -> Cell -> [String]
simpleTableTopPad cellStr height cell
  | length cell > height = error "SimpleTableGenerator: Cell's height is greater than maximum!"
  | length cell == height = cell
  | otherwise = padding ++ cell
    where
      padding = replicate (height - length cell) ""

simpleTableMiddlePad :: String -> Int -> Cell -> [String]
simpleTableMiddlePad cellStr height cell
  | length cell > height = error "SimpleTableGenerator: Cell's height is greater than maximum!"
  | length cell == height = cell
  | even (height - length cell) = halfPadding ++ cell ++ halfPadding
  | otherwise = halfPadding ++ cell ++ halfPadding ++ [""]
    where
      halfPadding = replicate ((height - length cell) `div` 2) ""

simpleTableAddVPadding :: (String -> Int -> Cell -> [String]) -> Int -> String -> Int -> Cell -> [String]
simpleTableAddVPadding paddingFunction n =
  (\ f cellStr height ->
    padding ++ (f cellStr height) ++ padding) . paddingFunction
  where
    padding = (concat $ take n $ repeat [""])

simpleTableAddHPadding :: (String -> Int -> String -> String) -> Int  -> String -> Int -> String -> String
simpleTableAddHPadding paddingFunction n =
  (\ f padStr width -> padding  ++ (f padStr width) ++ padding) . paddingFunction
  where
    padding = take n $ concat $ repeat " "

constructPaddingFunctions :: SimpleTableConfig -> SimpleTableConfig
constructPaddingFunctions config = config {
  padFunction = simpleTableAddHPadding (padFunction config) (horizontalPadding config),
  cellPadFunction = simpleTableAddVPadding (cellPadFunction config) (verticalPadding config),
  horizontalPadding = 0,
  verticalPadding = 0
  }

validateConfig config
  | 0 == length (paddingStr config) = error "SimpleTableGenerator: paddingStr is empty!"
  | 11 /= length (tableBorders config) = error "SimpleTableGenerator: tableBorders must be a string of 11 characters!"
  | 0 > horizontalPadding config = error "SimpleTableGenerator: horizontalPadding must be >= 0!"
  | 0 > verticalPadding config = error "SimpleTableGenerator: verticalPadding must be >= 0!"
  | otherwise = config

-- functions

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = (.) map map
