-- 8  使用我们在前面章节中定义的二叉树类型，
--    写一个函数用于确定一棵二叉树的高度．高度的定义是从根节点到叶子节点经过的最大节点数
--    举个例子，Empty 这棵树的高度是0; 
--    Node "x" Empty Empty 这棵树的高度是1; 
--    Node "x" Empty (Node "y" Empty Empty) 这棵树的高度是2
--    依此类推

data Tree a = Node a  (Tree a ) (Tree a )
            | Empty
              deriving (Show)

getDepth Empty               =  0
getDepth (Node _ left right) = 1 +  max   (getDepth left)  (getDepth right)

test  =  getDepth c
      where c = Node 1  (Node 2 Empty Empty)  (Node 3 (Node 4 Empty Empty ) Empty)