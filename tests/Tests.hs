module Tests (tests) where

import Distribution.TestSuite

tests :: IO [Test]
tests = return [ Test TestInstance
                 { run = return $ Finished Pass
                 , name = "succeeds"
                 , tags = []
                 , options = []
                 , setOption = \_ _ -> Left "What?"
                 }
               ]
