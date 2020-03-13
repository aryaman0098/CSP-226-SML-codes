(* Defining a data type of binary tree *)
datatype 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree

exception Wrong_Traversal
exception Empty_BTree
exception Empty_List

(* This functon prints the preorder traversal of the binary tree *)
fun preorder_bintree Empty = [NONE] 
|   preorder_bintree (Node(n, left, right)) = [SOME n] @ preorder_bintree left @ preorder_bintree right


(* This functon prints the postorder traversal of the binary tree *)
fun postorder_bintree Empty = [NONE]
|   postorder_bintree (Node(n, left, right)) = postorder_bintree left @ postorder_bintree right @ [SOME n]


(* Given a preorder of a binary tree this function returns the binary tree. It makes use of 3 utility functions namely helper 1, helper 2, helper 3. *)
fun RecoverFromPre L = let
                           fun helper1 [] L =  L
                           |   helper1 (h1 :: tail) L = if isSome(h1) then
                                                            helper1 tail (L @ [((Node(valOf(h1), Empty, Empty)), false)])      
                                                       else 
                                                            helper1 tail (L @ [(Empty, true)])


                           fun helper2 (h :: []) = raise Wrong_Traversal 
                           |  helper2 ((Node(x, _, _), false) :: (leftSubtree, true) :: (rightSubtree, true) :: tail) = (Node(x, leftSubtree, rightSubtree), true) :: tail
                           |  helper2 (h :: t) =(h :: (helper2 t))
                         
                           
                           
                           fun helper3 [] = raise Empty_List
                           |  helper3 L = if length L = 1 then L 
                                          else helper3 (helper2 L )                           
                           
                           
                            val [(btree,flag)] =helper3(helper1 L [])
                       
                       in btree
                       end 


(* Given a postorder of a binary tree this function returns the binary tree. It makes use of 1 utility functions namely mirrorTree *)
fun RecoverFromPost L = let 
                              fun mirrorTree Empty = Empty
                              |   mirrorTree (Node(n, left, right)) = (Node(n, mirrorTree right, mirrorTree left))
                              
                              val M = rev L
                              val btree = RecoverFromPre M
                              val btree2 = mirrorTree btree 
                           
                        in btree2

                        end

(* Given a function recoverFunction, tree, and pre/post order list of the tree, this funtion checks whether the function recoverFunction actually converts the given list into the tree or not. *)
fun check recoverFunction Empty [] = raise Empty_List 
|   check recoverFunction tree (head::tail) = let 
                                                val t = recoverFunction (head::tail)
                                              in 
                                                if t= tree then true
                                                else false
                                              end


