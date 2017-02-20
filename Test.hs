module Test ( tests ) where

import Distribution.TestSuite
import Text.SimpleTableGenerator

-- (config, table content, width, height)
sizeTests = [
  -- simpleTableConfig: horizontal padding is 1, vertical padding is 0
  (simpleTableConfig, [["a"]], 5, 3),
  (simpleTableConfig, [["a", "b"]], 9 , 3),
  (simpleTableConfig, [["a", "b"], ["c", "d"]], 9 , 5),
  -- removed element doesn't change the size in this case
  (simpleTableConfig, [["a", "b"], ["c"]], 9 , 5),
  (simpleTableConfig, [["a", "b"], []], 9 , 5),

  -- Some edge cases:

  -- empty tables are empty
  (simpleTableConfig, [[]], 0, 0),
  (simpleTableConfig, [[], []], 0, 0),
  (simpleTableConfig, [[], [], []], 0, 0),

  -- But if there is at least one string, it will be boxed
  (simpleTableConfig, [[""]], 4, 3),
  (simpleTableConfig, [[""], [""]], 4, 5),

  -- Newlines
  (simpleTableConfig, [["a\nb"]], 5, 4),
  (simpleTableConfig, [["a\nb", "c\nd"]], 9, 4)
  ];

testSizes tests = if null failed then
                    Nothing
                  else
                    Just failed
  where
    failed = filter (not.passed) tests
    passed tst = (length $ filter (\n -> n /= '\n') tbl) == h * w
      where
        config = (\(r, _, _, _) -> r) tst
        tblsource = (\(_, r,  _, _) -> r) tst
        w = (\(_, _, r, _) -> r) tst
        h = (\(_, _, _, r) -> r) tst
        tbl = makeSimpleTable config tblsource

getSizeOfTextTable txt = (lineLen, lineCount) -- w, h
  where
    lineLen = length $ takeWhile not_newline txt
    not_newline = (/='\n')
    wout_newlines = filter (not . (=='\n')) txt
    lineCount = (length $ wout_newlines) `div` lineLen

tests :: IO [Test]
tests = return [Test sizes]
  where
    reportSizesErrors [] = ""
    reportSizesErrors ((config, table, w, h) : xs) =
      "expected size: (" ++ show w ++ "," ++ show h ++ "), got: "
      ++ show (getSizeOfTextTable (makeSimpleTable config table)) ++ "; " ++ reportSizesErrors xs
    sizes = TestInstance
        { run = return $ case testSizes sizeTests of
                           Just errs -> Finished $ Fail $ reportSizesErrors errs
                           Nothing -> Finished Pass
        , name = "Check table sizes."
        , tags = []
        , options = []
        , setOption = \_ _ -> Right sizes
        }
