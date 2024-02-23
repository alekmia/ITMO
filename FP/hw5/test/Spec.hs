import Test.Hspec

import HW5.Base
import HW5.Action
import HW5.Evaluator
import HW5.Parser
import HW5.Pretty

import Data.Ratio
import Control.Monad.IO.Class
import Data.Set
import Data.Text as T
import qualified Data.Sequence as S
import Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import qualified Data.Map as M
-- import Data.ByteString.Internal as I

main :: IO ()
main = hspec $ do
    describe "T1 test" $ do
        it "parse test" $ do
            parse "1" `shouldBe` Right (HiExprValue (HiValueNumber (1 % 1)))
            parse "19" `shouldBe` Right (HiExprValue (HiValueNumber (19 % 1)))
            parse "-7" `shouldBe` Right (HiExprValue (HiValueNumber ((-7) % 1)))
            parse "6.99" `shouldBe` Right (HiExprValue (HiValueNumber (699 % 100)))
            parse "-3.14" `shouldBe` Right (HiExprValue (HiValueNumber ((-314) % 100)))
            parse "add" `shouldBe` Right (HiExprValue (HiValueFunction HiFunAdd))
            parse "div" `shouldBe` Right (HiExprValue (HiValueFunction HiFunDiv))
            parse "add(1, 2)" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (1 % 1)), HiExprValue (HiValueNumber (2 % 1))])
            parse "add(1, add(2, 3))" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (1 % 1)), HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (2 % 1)), HiExprValue (HiValueNumber (3 % 1))]])
        it "eval test" $ do
            evaled <- checkEval "1"
            evaled `shouldBe` HiValueNumber (1 % 1)
            evaled2 <- checkEval "add"
            evaled2 `shouldBe` HiValueFunction HiFunAdd
            evaled3 <- checkEval "add(1, 2)"
            evaled3 `shouldBe` HiValueNumber (3 % 1)
            evaled4 <- checkEval "mul(sub(4, 2), div(12, 3))"
            evaled4 `shouldBe` HiValueNumber (8 % 1)
            evaled5 <- checkEval "div(1, 2)"
            evaled5 `shouldBe` HiValueNumber (1 % 2)
        it "pretty test" $ do
            show (prettyValue (HiValueNumber (1 % 1))) `shouldBe` "1"
            show (prettyValue (HiValueNumber ((-3) % 1))) `shouldBe` "-3"
            show (prettyValue (HiValueNumber (1 % 3))) `shouldBe` "1/3"
            show (prettyValue (HiValueNumber ((-14) % 11))) `shouldBe` "-1 - 3/11"
            show (prettyValue (HiValueFunction HiFunAdd)) `shouldBe` "add"
            show (prettyValue (HiValueFunction HiFunMul)) `shouldBe` "mul"

    describe "T2 test" $ do
        it "parse test" $ do
            parse "true" `shouldBe` Right (HiExprValue (HiValueBool True))
            parse "greater-than" `shouldBe` Right (HiExprValue (HiValueFunction HiFunGreaterThan))
            parse "not(true)" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunNot)) [HiExprValue (HiValueBool True)])
            parse "if(10, true, 456)" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) [HiExprValue (HiValueNumber (10 % 1)),HiExprValue (HiValueBool True),HiExprValue (HiValueNumber (456 % 1))])
            parse "equals(equals(10, 10), 9)" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [HiExprValue (HiValueNumber (10 % 1)),HiExprValue (HiValueNumber (10 % 1))],HiExprValue (HiValueNumber (9 % 1))])
        it "eval test" $ do
            evaled <- checkEval "false"
            evaled `shouldBe` HiValueBool False
            evaled2 <- checkEval "not-less-than"
            evaled2 `shouldBe` HiValueFunction HiFunNotLessThan
            evaled3 <- checkEval "and(true, false)"
            evaled3 `shouldBe` HiValueBool False
            evaled4 <- checkEval "or(false, true)"
            evaled4 `shouldBe` HiValueBool True
            evaled5 <- checkEval "if(false, 3, 5)"
            evaled5 `shouldBe` HiValueNumber (5 % 1)
            evaled6 <- checkEval "equals(equals(10, 10), 9)"
            evaled6 `shouldBe` HiValueBool False
            evaled7 <- checkEval "not-greater-than(add(1, 2), 3)"
            evaled7 `shouldBe` HiValueBool True
            evaled8 <- checkEval "equals(add, add)"
            evaled8 `shouldBe` HiValueBool True
        it "pretty test" $ do
            show (prettyValue (HiValueBool False)) `shouldBe` "false"
            show (prettyValue (HiValueBool True)) `shouldBe` "true"
            show (prettyValue (HiValueFunction HiFunIf)) `shouldBe` "if"
            show (prettyValue (HiValueFunction HiFunNotGreaterThan)) `shouldBe` "not-greater-than"

    describe "T3 test" $ do
        it "parse test" $ do
            parse "1 * 1" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprValue (HiValueNumber (1 % 1)), HiExprValue (HiValueNumber (1 % 1))])
            parse "1 >= 1" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) [HiExprValue (HiValueNumber (1 % 1)),HiExprValue (HiValueNumber (1 % 1))])
            parse "2 + 2 * 3 == (2 + 2) * 3" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (2 % 1)),HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprValue (HiValueNumber (2 % 1)),HiExprValue (HiValueNumber (3 % 1))]],HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (2 % 1)),HiExprValue (HiValueNumber (2 % 1))],HiExprValue (HiValueNumber (3 % 1))]])
        it "eval test" $ do
            evaled <- checkEval "4 / 2"
            evaled `shouldBe` HiValueNumber (2 % 1)
            evaled2 <- checkEval "(2 + 2) * 3"
            evaled2 `shouldBe` HiValueNumber (12 % 1)
            evaled3 <- checkEval "10 == 2*5 && 143 == 11*13"
            evaled3 `shouldBe` HiValueBool True
    
    describe "T4 test" $ do
        it "parse test" $ do
            parse "\"Hello\"" `shouldBe` Right (HiExprValue (HiValueString (T.pack "Hello")))
            parse "null" `shouldBe` Right (HiExprValue HiValueNull)
            parse "length(\"Hello world\")" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunLength)) [HiExprValue (HiValueString (T.pack "Hello world"))])
            parse "to-lower(\"yo whatsup\")" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunToLower)) [HiExprValue (HiValueString (T.pack "yo whatsup"))])
            parse "to-upper(trim(\"Hello\"))" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunToUpper)) [HiExprApply (HiExprValue (HiValueFunction HiFunTrim)) [HiExprValue (HiValueString (T.pack "Hello"))]])
            parse "\"hello\"(1)" `shouldBe` Right (HiExprApply (HiExprValue (HiValueString (T.pack "hello"))) [HiExprValue (HiValueNumber (1 % 1))])
        it "eval test" $ do
            evaled <- checkEval "\"hello\"(1)"
            evaled `shouldBe` HiValueString (T.singleton 'e')
            evaled2 <- checkEval "length(\"Hello world\")"
            evaled2 `shouldBe` HiValueNumber (11 % 1)
            evaled3 <- checkEval "to-upper(\"Hello world\")"
            evaled3 `shouldBe` HiValueString (T.pack "HELLO WORLD")
            evaled4 <- checkEval "trim(\"   Hello world      \")"
            evaled4 `shouldBe` HiValueString (T.pack "Hello world")
            evaled5 <- checkEval "reverse(\"A nut for a jar of tuna\")"
            evaled5 `shouldBe` HiValueString (T.pack "anut fo raj a rof tun A")
        it "pretty test" $ do
            show (prettyValue (HiValueString (T.pack "alekmia"))) `shouldBe` "\"alekmia\""
            show (prettyValue (HiValueFunction HiFunLength)) `shouldBe` "length"

    describe "T4adv test" $ do
        it "eval test" $ do
            evaled <- checkEval "\"alekmia\"(0, -3)"
            evaled `shouldBe` HiValueString (T.pack "alek")
            evaled2 <- checkEval "\"alekmia\"(-3, -1)"
            evaled2 `shouldBe` HiValueString (T.pack "mi")
            evaled3 <- checkEval "\"alekmia\"(4, null)"
            evaled3 `shouldBe` HiValueString (T.pack "mia")
            evaled4 <- checkEval "\"alekmia\"(null, 4)"
            evaled4 `shouldBe` HiValueString (T.pack "alek")

    describe "T5 test" $ do
        it "parse test" $ do
            parse "range" `shouldBe` Right (HiExprValue (HiValueFunction HiFunRange))
            parse "[1, 2, 3]" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunList)) [HiExprValue (HiValueNumber (1 % 1)),HiExprValue (HiValueNumber (2 % 1)),HiExprValue (HiValueNumber (3 % 1))])
            parse "[]" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunList)) [])
        it "eval test" $ do
            evaled <- checkEval "[1, 2, 3]"
            evaled `shouldBe` HiValueList (S.fromList [HiValueNumber (1 % 1), HiValueNumber (2 % 1), HiValueNumber (3 % 1)])
            evaled2 <- checkEval "list(1, 2, 3)"
            evaled2 `shouldBe` HiValueList (S.fromList [HiValueNumber (1 % 1), HiValueNumber (2 % 1), HiValueNumber (3 % 1)])
            evaled3 <- checkEval "range(1, 2.3)"
            evaled3 `shouldBe` HiValueList (S.fromList [HiValueNumber (1 % 1), HiValueNumber (2 % 1)])
            evaled4 <- checkEval "range(1, 2.7)"
            evaled4 `shouldBe` HiValueList (S.fromList [HiValueNumber (1 % 1), HiValueNumber (2 % 1), HiValueNumber (3 % 1)])
            evaled5 <- checkEval "fold(add, [[1, 2], [3, 4, 5], [6]])"
            evaled5 `shouldBe` HiValueList (S.fromList [HiValueNumber (1 % 1), HiValueNumber (2 % 1), HiValueNumber (3 % 1), HiValueNumber (4 % 1), HiValueNumber (5 % 1), HiValueNumber (6 % 1)]) 
            evaled6 <- checkEval "[6, 9] * 3"
            evaled6 `shouldBe` HiValueList (S.fromList [HiValueNumber (6 % 1), HiValueNumber (9 % 1), HiValueNumber (6 % 1), HiValueNumber (9 % 1), HiValueNumber (6 % 1), HiValueNumber (9 % 1)]) 
            evaled7 <- checkEval "[\"alek\", \"mia\", 1](0, 2)"
            evaled7 `shouldBe`HiValueList (S.fromList [HiValueString (T.pack "alek"), HiValueString (T.pack "mia")])
        it "pretty test" $ do
            forPretty <- checkEval "[6, 9] * 3"
            show (prettyValue forPretty) `shouldBe` "[6, 9, 6, 9, 6, 9]"
            show (prettyValue (HiValueFunction HiFunFold)) `shouldBe` "fold"

    describe "T6 test" $ do
        it "parse test" $ do
            parse "pack-bytes" `shouldBe` Right (HiExprValue (HiValueFunction HiFunPackBytes))
            parse "[# 11 22 1f #]" `shouldBe` Right (HiExprValue (HiValueBytes (packByteString "\DC1\"\US")))
            parse "[# #]" `shouldBe` Right (HiExprValue (HiValueBytes B.empty))
            parse "encode-utf8([# 11 22 1f #])" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunEncodeUtf8)) [HiExprValue (HiValueBytes (packByteString "\DC1\"\US"))])
        it "eval test" $ do
            evaled <- checkEval "pack-bytes"
            evaled `shouldBe` HiValueFunction HiFunPackBytes
            evaled2 <- checkEval "[# 11 22 1f #]"
            evaled2 `shouldBe` HiValueBytes (packByteString "\DC1\"\US")
            evaled3 <- checkEval "decode-utf8([# 24 25 26 #])"
            evaled3 `shouldBe` HiValueString (T.pack "$%&")
            evaled4 <- checkEval "unpack-bytes([# 24 25 26 #])"
            evaled4 `shouldBe` HiValueList (S.fromList [HiValueNumber (36 % 1), HiValueNumber (37 % 1), HiValueNumber (38 % 1)])
            evaled5 <- checkEval "unzip(zip([# 11 22 1f #]))"
            evaled5 `shouldBe` HiValueBytes (packByteString "\DC1\"\US")
            evaled6 <- checkEval "deserialise(serialise(1))"
            evaled6 `shouldBe` HiValueNumber (1 % 1)
        it "pretty test" $ do
            show (prettyValue (HiValueBytes (packByteString "\DC1\"\US"))) `shouldBe` "[# 11 22 1f #]"
            show (prettyValue (HiValueFunction HiFunSerialise)) `shouldBe` "serialise"          

    describe "T7 test" $ do
        it "parse test" $ do
            parse "cd(\"alekmia\")" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunChDir)) [HiExprValue (HiValueString (T.pack "alekmia"))])
            parse "cd(\"alekmia\")!" `shouldBe` Right (HiExprRun (HiExprApply (HiExprValue (HiValueFunction HiFunChDir)) [HiExprValue (HiValueString (T.pack "alekmia"))]))
            parse "cwd" `shouldBe` Right (HiExprValue (HiValueAction HiActionCwd))
        it "eval test" $ do
            evaled <- checkEval "cwd"
            evaled `shouldBe` HiValueAction HiActionCwd
            evaled4 <- checkEval "cd(\"alekmia\")"
            evaled4 `shouldBe` HiValueAction (HiActionChDir "alekmia")
        it "pretty test" $ do
            show (prettyValue (HiValueAction HiActionCwd)) `shouldBe` "cwd"
            show (prettyValue (HiValueAction (HiActionMkDir "alekmia"))) `shouldBe` "mkdir(\"alekmia\")"
            show (prettyValue (HiValueFunction HiFunChDir)) `shouldBe` "cd"    

    describe "T8 test" $ do
        it "parse test" $ do
            parse "now" `shouldBe` Right (HiExprValue (HiValueAction HiActionNow))
            parse "now!" `shouldBe` Right (HiExprRun (HiExprValue (HiValueAction HiActionNow)))
            parse "parse-time(\"2004-11-21 00:00:00 UTC\") + 12 * 60 * 60" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprApply (HiExprValue (HiValueFunction HiFunParseTime)) [HiExprValue (HiValueString (T.pack "2004-11-21 00:00:00 UTC"))],HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprValue (HiValueNumber (12 % 1)),HiExprValue (HiValueNumber (60 % 1))],HiExprValue (HiValueNumber (60 % 1))]])
        it "eval test" $ do
            evaled <- checkEval "now"
            evaled `shouldBe` HiValueAction HiActionNow
            evaled2 <- checkEval "parse-time(\"2004-11-21 00:00:00 UTC\")"
            evaled2 `shouldBe` HiValueTime (UTCTime (fromGregorian 2004 11 21) (secondsToDiffTime 0))
            evaled3 <- checkEval "parse-time(\"2004-11-21 00:00:00 UTC\") + 12 * 60 * 60"
            evaled3 `shouldBe` HiValueTime (UTCTime (fromGregorian 2004 11 21) (secondsToDiffTime 43200))
        it "pretty test" $ do
            show (prettyValue (HiValueTime (UTCTime (fromGregorian 2004 11 21) (secondsToDiffTime 43200)))) `shouldBe` "parse-time(\"2004-11-21 12:00:00 UTC\")"
            show (prettyValue (HiValueFunction HiFunParseTime)) `shouldBe` "parse-time"     

    describe "T9 test" $ do
        it "parse test" $ do
            parse "rand" `shouldBe` Right (HiExprValue (HiValueFunction HiFunRand))
            parse "rand!" `shouldBe` Right (HiExprRun (HiExprValue (HiValueFunction HiFunRand)))
            parse "rand(0, 14)" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunRand)) [HiExprValue (HiValueNumber (0 % 1)),HiExprValue (HiValueNumber (14 % 1))])
            parse "rand(0, 14)!" `shouldBe` Right (HiExprRun (HiExprApply (HiExprValue (HiValueFunction HiFunRand)) [HiExprValue (HiValueNumber (0 % 1)),HiExprValue (HiValueNumber (14 % 1))]))
        it "eval test" $ do
            evaled <- checkEval "rand"
            evaled `shouldBe` HiValueFunction HiFunRand
            evaled2 <- checkEval "rand(0, 14)"
            evaled2 `shouldBe` HiValueAction (HiActionRand 0 14)
        it "pretty test" $ do
            show (prettyValue (HiValueFunction HiFunRand)) `shouldBe` "rand"

    describe "T10 test" $ do
        it "parse test" $ do
            parse "echo" `shouldBe` Right (HiExprValue (HiValueFunction HiFunEcho))
            parse "echo!" `shouldBe` Right (HiExprRun (HiExprValue (HiValueFunction HiFunEcho)))
            parse "echo(\"alekmia\")" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunEcho)) [HiExprValue (HiValueString (T.pack "alekmia"))])
            parse "echo(\"alekmia\")!" `shouldBe` Right (HiExprRun (HiExprApply (HiExprValue (HiValueFunction HiFunEcho)) [HiExprValue (HiValueString (T.pack "alekmia"))]))
        it "eval test" $ do
            evaled <- checkEval "echo"
            evaled `shouldBe` HiValueFunction HiFunEcho
            evaled2 <- checkEval "echo(\"alekmia\")"
            evaled2 `shouldBe` HiValueAction (HiActionEcho (T.pack "alekmia"))
            evaled3 <- checkEval "echo(\"alekmia\")!"
            evaled3 `shouldBe` HiValueNull
        it "lazy test" $ do
            evaled <- checkEval "true || div(1, 0)"
            evaled `shouldBe` HiValueBool True
            evaled2 <- checkEval "false && div(1, 0)"
            evaled2 `shouldBe` HiValueBool False
        it "pretty test" $ do
            show (prettyValue (HiValueFunction HiFunEcho)) `shouldBe` "echo"
        

    describe "T11 test" $ do
        it "parse test" $ do
            parse "{ \"age\": 99, \"points\": 0 }" `shouldBe` Right (HiExprDict [(HiExprValue (HiValueString (T.pack "age")),HiExprValue (HiValueNumber (99 % 1))),(HiExprValue (HiValueString (T.pack "points")),HiExprValue (HiValueNumber (0 % 1)))])
            parse "count" `shouldBe` Right (HiExprValue (HiValueFunction HiFunCount))
            parse "{ \"age\": 99, \"points\": 0 }(\"age\")" `shouldBe` Right (HiExprApply (HiExprDict [(HiExprValue (HiValueString (T.pack "age")),HiExprValue (HiValueNumber (99 % 1))),(HiExprValue (HiValueString (T.pack "points")),HiExprValue (HiValueNumber (0 % 1)))]) [HiExprValue (HiValueString (T.pack "age"))])
            parse "count([# 11 22 33 11 11 #])" `shouldBe` Right (HiExprApply (HiExprValue (HiValueFunction HiFunCount)) [HiExprValue (HiValueBytes (packByteString "\DC1\"3\DC1\DC1"))])
        it "eval test" $ do
            evaled <- checkEval "invert"
            evaled `shouldBe` HiValueFunction HiFunInvert
            evaled2 <- checkEval "{ \"age\": 99, \"points\": 0 }"
            evaled2 `shouldBe` HiValueDict (M.fromList [(HiValueString (T.pack "age"),HiValueNumber (99 % 1)),(HiValueString (T.pack "points"),HiValueNumber (0 % 1))])
        it "pretty test" $ do
            show (prettyValue (HiValueDict (M.fromList [(HiValueString (T.pack "age"),HiValueNumber (99 % 1)),(HiValueString (T.pack "points"),HiValueNumber (0 % 1))]))) `shouldBe` "{ \"age\": 99, \"points\": 0 }"
            show (prettyValue (HiValueFunction HiFunKeys)) `shouldBe` "keys"
    
            
        
checkEval :: String -> IO HiValue
checkEval str = do
    let parsed = parse str
    case parsed of
        Left _ -> return HiValueNull
        Right a -> do
            evaled <- liftIO (runHIO (eval a) (fromList [AllowWrite, AllowRead, AllowTime]))
            case evaled of
                Left _ -> return HiValueNull
                Right b -> return b

packByteString :: String -> B.ByteString
packByteString str = encodeUtf8 (T.pack str)