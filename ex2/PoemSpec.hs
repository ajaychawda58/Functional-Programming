module PoemSpec (spec)
where

import Test.Hspec
import qualified Poem
import qualified Data.Text as T
strip  = T.unpack . T.strip . T.pack

poemText = "This old man, he played one,\nHe played knick-knack on my thumb;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played two,\nHe played knick-knack on my shoe;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played three,\nHe played knick-knack on my knee;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played four,\nHe played knick-knack on my door;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played five,\nHe played knick-knack on my hive;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played six,\nHe played knick-knack on my sticks;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played seven,\nHe played knick-knack up in heaven;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played eight,\nHe played knick-knack on my gate;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played nine,\nHe played knick-knack on my spine;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\nThis old man, he played ten,\nHe played knick-knack once again;\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home."

spec = do
    describe "thisOldMan" $ do
        it "has the correct value" $
            strip Poem.thisOldMan `shouldBe` poemText

main = hspec spec
