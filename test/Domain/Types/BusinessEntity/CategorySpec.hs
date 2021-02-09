module Domain.Types.BusinessEntity.CategorySpec where

import ClassyPrelude
import Test.Hspec
import Domain.Types.BusinessEntity.Category


spec :: Spec
spec = do
    describe "convert CategoryRaw -> Category" $ do
        it "simple convert"  $ do
            convertCategoryRawArray [CategoryRaw 1 "test" Nothing] `shouldBe` [Category 1 "test" Nothing]
        it "one chain category"  $ do
            convertCategoryRawArray [
                    CategoryRaw 1 "test1" Nothing,
                    CategoryRaw 2 "test2" (Just 1),
                    CategoryRaw 3 "test3" (Just 2),
                    CategoryRaw 4 "test4" (Just 3)
                    ] `shouldBe` [
                        Category 4 "test4" (Just $  Category 3 "test3" (Just $ Category 2 "test2" (Just $ Category 1 "test1" Nothing)))
                        ]
        it "return 6 category"  $ do
            convertCategoryRawArray [
                    CategoryRaw 1 "1" Nothing, 
                    CategoryRaw 2 "2-1" (Just 1), 
                    CategoryRaw 3 "3-1" (Just 1), 
                    CategoryRaw 4 "4-3-1" (Just 3), 
                    CategoryRaw 5 "test5-4-3-1" (Just 4), 
                    CategoryRaw 6 "test6-2-1" (Just 2), 
                    CategoryRaw 7 "test7-1" (Just 1), 
                    CategoryRaw 8 "8-2-1" (Just 2), 
                    CategoryRaw 9 "test9-8-2-1" (Just 8), 
                    CategoryRaw 10 "test10-1" (Just 1), 
                    CategoryRaw 11 "test11-1" (Just 1) 
                    ] `shouldBe` [
                            Category {idCategory = 6, nameCategory = "test6-2-1", parentCategory = Just (Category {idCategory = 2, nameCategory = "2-1", parentCategory = Just (Category {idCategory = 1, nameCategory = "1", parentCategory = Nothing})})},
                            Category {idCategory = 11, nameCategory = "test11-1", parentCategory = Just (Category {idCategory = 1, nameCategory = "1", parentCategory = Nothing})},
                            Category {idCategory = 10, nameCategory = "test10-1", parentCategory = Just (Category {idCategory = 1, nameCategory = "1", parentCategory = Nothing})},
                            Category {idCategory = 7, nameCategory = "test7-1", parentCategory = Just (Category {idCategory = 1, nameCategory = "1", parentCategory = Nothing})},
                            Category {idCategory = 5, nameCategory = "test5-4-3-1", parentCategory = Just (Category {idCategory = 4, nameCategory = "4-3-1", parentCategory = Just (Category {idCategory = 3, nameCategory = "3-1", parentCategory = Just (Category {idCategory = 1, nameCategory = "1", parentCategory = Nothing})})})},
                            Category {idCategory = 9, nameCategory = "test9-8-2-1", parentCategory = Just (Category {idCategory = 8, nameCategory = "8-2-1", parentCategory = Just (Category {idCategory = 2, nameCategory = "2-1", parentCategory = Just (Category {idCategory = 1, nameCategory = "1", parentCategory = Nothing})})})}
                        ]