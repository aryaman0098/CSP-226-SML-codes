(* To raise an exception if wrong input has been given *)

exception negative

(* Utility function for finding the square of a number *)

fun square j=j*j

(* Utility function to find the floor function of the square root of a number *)

fun intsqrt n=
	if n<0 then raise negative
	else if n=0 then 0
	else (* n>0 *)
		let val m=n div 4
		    val i=intsqrt m
		    val twoi =2*i
		    val twoiplus1=2*i+1
		in if square (twoiplus1)<= n then twoiplus1
		    else twoi
		end 


(* Utility function to find the successor of the points (x,y) in the path of the bug *)

fun successor (x,y) =
    if(x=0 andalso y=0) 
        then (0,1)
    else if(x-y<0 andalso x+y-1>=0) 
        then (x+1,y)
    else if(x-y<=0 andalso x+y-1<0) 
        then (x,y+1)
    else if(x-y>0 andalso x+y<=0) 
        then (x-1,y)
    else if(x+y>0 andalso x-y>=0) 
        then (x,y-1)
    else 
        raise negative



(* Analytical approach to find the number of steps required to reach the co-ordinate points (x,y) *)

fun FijAnalytical(x:int, y:int) = 
    if(x=0 andalso y=0)
        then 0
    else if(x-y<=0 andalso x+y-1>0) then
        let
            val n= 4*square(y)-2*y
        in
            n-(y-x)
        end
    else if(x-y<0 andalso x+y-1<=0) then
        let 
            val n=4*square(x)+2*(~x)
        in 
            n+(~x+y)
        end
    else if(x-y>=0 andalso x+y<0) then       (* Taking all the regions into consideration for finding the number of the steps *)
        let 
            val n=4*square(y)
        in  
            n-(y+x)
        end
    else if(x+y>=0 andalso x-y>0) then
        let 
            val n=4*square(x)-2*x
        in
            n+(x-y)
        end 
    else
        raise negative


(* Recursive approach to find the number of steps required to reach the co-ordinate points (x,y) *)

fun FijRecursive  (x:int, y:int ) =
    if(x=0 andalso y=0) 
        then 0
    else if(x-y<=0 andalso x+y-1>0) 
        then 1+FijRecursive (x-1,y)
    else if(x-y<0 andalso x+y-1<=0) 
        then 1+FijRecursive (x,y-1)
    else if(x-y>=0 andalso x+y<0) 
        then 1+FijRecursive (x+1,y)
    else if(x+y>=0 andalso x-y>0) 
        then 1+FijRecursive (x,y+1)
    else 
        raise negative


(* Analytical approach to find the co-ordinates reached after k steps *)

fun   FkAnalytical k= 
    let
		val n=intsqrt (k)
	in	
		if n mod 2 = 0 andalso (square(n)+n) > k andalso k>=(square(n)) then
			
			let
				val x=n div 2
				val y= ~(n div 2)
			in
				(x-(k-square(n)),y)
			end

		else if n mod 2 = 0 andalso square(n+1)>k andalso k>=(square(n)+n) then
			let	
				val x=n div 2
				val y= ~(n div 2)
			in
				(x-n,y+((n+1)-(square(n+1)-k)))
			end

		else if n mod 2 = 1 andalso (square(n)+n>k) andalso k>=square(n) then       (* Taking into considerations all the regions *)
			let
				val x= ~(n-1) div 2
				val y=(n+1) div 2
			in	
				(x+(k-square(n)),y)
			end

		else if n mod 2 =1 andalso square(n+1)>k andalso k>=(square(n)+n) then
			let
				val x= ~(n-1) div 2
				val y=(n+1) div 2
			in	
				(x+n,y-((n+1)-(square(n+1)-k)))
			end
		else	
			raise negative
	end

 
(* Recursive approach to findthe co-ordinates reached after k steps *)

fun  FkRecursive k=
    if(k=0)
        then (0,0)
    else 
        successor( FkRecursive(k-1))


    
