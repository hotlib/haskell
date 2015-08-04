-- example of reading and writing XMLs using HaXML

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe 
import Text.XML.HaXml.Types

data Students = Students {enrolled :: [Student]} deriving Show
data Student = Student {name :: String, gender :: String, address :: Address} deriving Show
data Address = Address {street :: String, number :: Int} deriving Show

main :: IO ()
main = do
	stds <- fReadXml "sample-in.xml"::IO Students
	print stds
        fWriteXml "sample-out.xml" stds

attrToText :: String -> [Attribute] -> Maybe String
attrToText n as = foldl1 (<|>) attrText
       where attrText = map (fromAttrToStr n) as

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

instance HTypeable Students where
   toHType (Students ss) = Defined "Students" [] [Constr "Students" [] [toHType ss]]

instance HTypeable Address where
    toHType (Address s n) = Defined "Address" [] [Constr "Address" [] [toHType s, toHType n]]

instance HTypeable Student where
    toHType s =
        let Student n g a = s in Defined "Student" [] [Constr "Student" [] [toHType n, toHType g, toHType a]]

instance XmlContent Student where
    parseContents = do 
	     e <- element ["Student"]
             interior e (Student <$> parseName <*> parseGender e <*> parseContents ) 
	    where 
               parseName = inElement "name" text
               parseGender e = return $ fromMaybe "unknown" $ attrToText "gender" $ attrs e 

    toContents v@(Student n g a) =
	[mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "gender" g]  ( mkElemC "name" (toText n) : toContents a)]

instance XmlContent Students where
    parseContents = inElement "Students" (Students <$> parseContents)
 
    toContents v@(Students ss) =
         [mkElemC (showConstr 0 $ toHType v) (toContents ss)]

instance XmlContent Address where
    parseContents = inElement "Address" (Address <$> parseStreet <*> parseNumber)
 		where
                  parseStreet = inElement "street" text
                  parseNumber = read <$> inElement "number" text		  

    toContents a@(Address s n) =
         [mkElemC (showConstr 0 $ toHType a) [mkElemC "street" $ toText s, mkElemC "number" $ toText . show $ n]]

