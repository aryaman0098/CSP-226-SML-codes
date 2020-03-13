(* Example of a sorting code *)

fun sort R []=[]    
   |    sort R (h::t)=
        let fun insert R [] x=[x]
              |     insert R (h::t) x=
                    if R (x,h) then x::(h::t)
                    else h::(insert R t x)
                val rest = sort R t
        in  insert R rest h
        end; 


   

(* Utility function that deletes the first occurrance of an element from the list and returns the remaining list *)

fun delete x L =
    case L of
    []=>[]
    |  head::tail =>if x=head   
                            then tail
                    else 
                            head::(delete x tail)


(* Utility function that tells whether an element 'x' is present in the list L. It retruns a boolean value *)

fun isPresent x L=
    case L of
    []=>false
    | head::tail=>if x=head
                        then true 
                  else 
                        isPresent x tail


(* Utility function that compares the length of two lists and returns true is their length is same else returns false *)

fun lengthCompare M L=
    if(length M = length L)
        then true
    else 
        false





(* This function takes sorting function 'sort', Relation to be applied 'R' and input list 'L' and checks whether the sorting function actually sorts the given list according to the given relation or not *)

fun sortChecker sort R L=
  let 
        val M=sort R L
        
        (* Checks whether the two input lists are permutation of each other or not *)
        fun permutationChecker M L=
            case M of
            []=>if(L<>nil) then false else true
            | head::tail=>  
                    if L=[]
                        then true
                    else if isPresent head L  andalso lengthCompare M L  then
                        permutationChecker tail (delete head L)
                    else 
                        false 

        (* Checks whether the input relation is followed by the adjacent elements if the list or not *)
        fun logicChecker L R =
            case L of
            []=>true
            |  head::tail=>
                            if (length L=1)
                                 then true
                            else if(length L=2 andalso (R(head, hd tail) orelse head=hd tail))
                                then true
                            else if R(head, hd tail) orelse (head=hd tail) 
                                then logicChecker tail R
                            else 
                                false 
    
    in  
            if permutationChecker M L  andalso logicChecker M R    (* Checking whether permutationChecker and logicChecker yield true values of not *)
                then true
            else 
                false
    end
