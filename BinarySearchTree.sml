(* Creating a data-type that is Binary Search Tree  *)
datatype 'a bst = Empty | Node of 'a bst * 'a * 'a bst


(* Exception Handling *)
exception Empty_BST
exception Empty_List
exception Exception
exception Element_Not_Found


(* Checks whether the BST is empty or not *)
fun isEmpty Empty =true
|   isEmpty _     =false


(* This function takes a relation, value and a BST and inserts the value at appropriate position in the BST *)
fun insertInBST R x tree = 
     case tree of
     Empty => Node(Empty,x,Empty)
    |  Node(left, n, right) =>
            if R(x,n) then  Node((insertInBST R x left), n, right)
            else  Node(left, n ,(insertInBST R x right))


(* Takes a list and converts it into a BST element by element *)
fun listToBST R [] tree  = tree 
    | listToBST R (h::t) tree =  listToBST R t (insertInBST R h tree)


(* Deletes the minimum element of the BST w.r.t relation R *)
fun deleteMin  tree  = 
    case tree of
    Empty => raise Empty_BST
    |   Node(Empty, n, right) =>if right <> Empty then  right
                                else Empty
    |   Node(left, n, Empty) => Node(deleteMin  left, n, Empty)
    |   Node(left, n, right) => Node(deleteMin  left, n, right)


(* This function stores the inorder traversal of the BST in a list *)
fun inorderTraversal Empty = nil
    |   inorderTraversal (Node(left, n, right)) = inorderTraversal (left) @ [n] @ inorderTraversal (right)


(* Takes a value x and finds the corresponding node with that value and delete it. It makes use of the utility function delMin *)
fun delNode R x tree =
    case tree of 
    Empty => raise Empty_BST
    |   Node(left, n, right) => let  fun delMin  tree  = 
                                        case tree of
                                        Empty => raise Empty_BST
                                        |   Node(Empty, n, right) =>if right <> Empty then  (n, right)
                                                                      else (n, right)
                                        |   Node(left, n, Empty) => delMin left
                                        |   Node(left, n, right) => delMin left
                                    
                                in  if R(x,n) then Node(delNode R x left, n, right)
                                    
                                    else if n=x then 
                                        if left=Empty andalso right=Empty then Empty
                                        else if left=Empty andalso right<>Empty then right
                                        else if left<>Empty andalso right=Empty then left
                                        else let 
                                                 val (m, Right) = delMin right
                                             in   Node(left, m, Right)
                                             end
                                    
                                    else Node(left, n , delNode R x right)
                                end


(* This function takes a tree as an input and return true if it is a Binary Search Tree, else return false. It makes use of two other function namely chkLeft and chkRight which compare the root of the tree with the left subtree and the right subtree *)
fun bstCheck R tree = 
    case tree of 
    Empty => true
    |   Node(left, n, right)=> let 
                                    fun chkLeft R x Empty =true
                                    |  chkLeft R x (Node(left, n, right))= let
                                                                                val r = R(n,x)
                                                                            in  
                                                                            r andalso chkLeft R x left andalso chkLeft R x right
                                                                            end 
                                    fun chkRight R x Empty =true
                                    |   chkRight R x (Node(left, n, right))= let
                                                                                val r = R(x,n)
                                                                             in  
                                                                             r andalso chkRight R x left andalso chkRight R x right
                                                                             end

                                in (chkLeft R n left) andalso (chkRight R n right) andalso (bstCheck R left) andalso (bstCheck R right)  
                                
                                end




(* (Node((Node((Node(Empty,1,(Node((Node(Empty,2,Empty)),3,Empty)))),6,(Node(Empty,7,Empty)))),8,Empty)) *)