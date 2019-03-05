open Tetramino

open Graphics

type grille = {
	g: int array array;
	w:int;
	h:int
}
(*grille tetris 20*14*)
let make w h =
	let g= Array.make h (Array.make 0 0) in
	Array.iteri (fun i _ -> g.(i) <- Array.make w 0 ) g ;
	{g = g; w=w; h=h}

  
let afficheBoard b  =
  for i=0 to (b.h-1) do
    for j=0 to (b.w-1) do
      if(b.g.(i).(j)=0) then (
        (set_color white);
      	(fill_rect ((j*20)+200) (10+((19-i))*20) 20 20);
      	print_int  0;
      	Printf.printf "  %!";
      	)
      else if (b.g.(i).(j)=1) then (
      	(set_color blue);
      	(fill_rect (200+(j*20)) (10+((19-i)*20)) 20 20);
      	print_int  1;
      	Printf.printf "  %!";)
      else(
      	(set_color red);
      	(fill_rect (200+(j*20)) (10+((19-i)*20)) 20 20);
      	print_int 2;
      	Printf.printf "  %!";)
      	

    done;
    Printf.printf "  \n\n%!";
  done
  

	
	
	
let fini b=
	let test=ref false in
	for j=0 to (b.w - 1) do
		if b.g.(0).(j)=2 then test:=true
	done;
	!test
		
let collision board t=
	let test=ref false in
	for k=0 to 3 do
		let (a,b)= (List.nth t.squares k)in
		if (!test=false && (b=(board.h - 1) || board.g.(b+1).(a)=2)) then(
		for k'=0 to 3 do
			let (a',b')= (List.nth t.squares k')in
			board.g.(b').(a')<-(2)
		done;
		test:=true)
	done;
	!test 

let rotate (tetra:tetramino) :unit=
  if (tetra.k = Square)then ()
  else(
  let a,b = (List.nth tetra.squares 1) in
  (*tetra.squares<-List.map (fun (x,y)->(-1*(y-b)),(x-a)) tetra.squares*)
  let x,y = (List.nth tetra.squares 0) in
  let x',y' = (List.nth tetra.squares 2) in
  let x'',y'' = (List.nth tetra.squares 3) in
  let liste=[((-1*(y-b))+a,(x-a)+b);(a,b);((-1*(y'-b))+a,(x'-a)+b);((-1*(y''-b))+a,(x''-a)+b)]in
  tetra.squares<-liste;
  for k = 0 to 3 do 
  	let (a',b')=(List.nth tetra.squares k)in
	if (a'> 13 ) then (left tetra)
	else if (a'< 0 ) then (right tetra)
  done)
  
 let dessinePieceSuivante tetra =
  set_color black;
  fill_rect 500 500 300 300;
  set_color white;
  fill_rect 550 550 200 200;
  set_color black;
  for i=0 to 3 do
    match List.nth tetra.squares i with
    |x,y -> fill_rect (510+20*x) (620+20*(4-y)) 20 20
    |_ -> raise (Invalid_argument "wrong")
  done
    
let prepare_random () =
  let newTetra = ref (make_random ()) in
  
  dessinePieceSuivante !newTetra;
  newTetra 
let rec update (tetra:tetramino) f board cond=
	if cond then(
		let tmp'= ref true in
		let clone= {tetra with squares=tetra.squares} in
		(f clone);
		for k=0 to 3 do
			let (a,b)= (List.nth clone.squares k)in
			if((a<=13 && board.g.(b).(a)=2) || (a>=0 && board.g.(b).(a)=2))then tmp':=false 
		done;
		if(!tmp') then (
			for k=0 to 3 do
				let (a,b)= (List.nth tetra.squares k)in
				board.g.(b).(a)<-(0)
			done;
			(f tetra);
			for k=0 to 3 do
				let (a,b)= (List.nth tetra.squares k)in
				board.g.(b).(a)<-(1)
			done;
			(collision board tetra)
			)
		else (collision board tetra)
	)	
	else(	
	let tmp= ref true in
	if not (collision board tetra) then (
		let clone= {tetra with squares=tetra.squares} in
		(f clone);
		for k=0 to 3 do
			let (a,b)= (List.nth clone.squares k)in
			if((a<=13 && board.g.(b).(a)=2) || (a>=0 && board.g.(b).(a)=2))then tmp:=false 
		done;
		if(!tmp) then (
			for k=0 to 3 do
				let (a,b)= (List.nth tetra.squares k)in
				board.g.(b).(a)<-(0)
			done;
			(f tetra);
			for k=0 to 3 do
				let (a,b)= (List.nth tetra.squares k)in
				board.g.(b).(a)<-(1)
			done;
			false
			)
		else false
		)
	else(
		let time =  ref (Unix.gettimeofday ()) in
		let time'=  ref (Unix.gettimeofday ()) in 
		Printf.printf("ok\n");
		while ((!time' -. !time ) < 0.3) do
			(*show !t;
		*)	time':=(Unix.gettimeofday ());
			let e =Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Poll] in 
			if e.Graphics.keypressed then begin
				match Graphics.read_key () with
				| '4' -> tmp:=(update tetra left board true) ;
				| '6' -> tmp:=(update tetra right board true);
				| _ -> ();
			end;
		done;
		true
	))
	
		
(*fonction delete_line*)
let getFullLine b = (* renvoie l indice de la ligne ou -1 si aucune ligne pleine *)
  let test=ref false in
  let y =ref (-1) in
  for i=0 to b.h-1 do
    test:=false;
    for j=0 to b.w-1 do
      if b.g.(i).(j) == 1 || b.g.(i).(j) == 0 then test:= true    (*test = true si il y a un 1 ou un 0 dans la ligne *)
    done;
    if (not !test) then y := i
  done;
  !y

let deleteFirstLine b =
  for j=0 to b.w-1 do
    b.g.(0).(j)<-0           
  done

let deleteLineI b line =
  for j=0 to b.w-1 do
    b.g.(line).(j)<- 0           
  done

let avalancheToI b line =
  for i=line downto 1 do
    for j=0 to (b.w - 1) do
      b.g.(i).(j)<-b.g.(i-1).(j)           
    done;
  done
   
let deleteAllFullLines b =
  let line= ref (getFullLine b) in (*y=-2 a l initialisation*)
  while (!line <> (-1))do
    Printf.printf "  found %!";
    if (!line >=0) then(
      print_int !line;
      (*deleteFirstLine b;
*)
      deleteLineI b !line;
      avalancheToI b !line
    );
    line:=(getFullLine b)
  done

let show tetra=
	for k=0 to 3 do
		let (a,b)= (List.nth tetra.squares k)in
		(fill_rect (20+a*20) (600+ b*20)  20 20)
	done
	
(* fin delete line*)
let _ =
	let b = make 14 20 in
	open_graph " 800x800";	
	(fill_rect 0 0 800 800);
	let t=ref  (make_random ()) in
	while not (fini b) do
	  	let t'=ref !(prepare_random()) in

		let tombe=ref false in
		while not !tombe do	
			(*move t;*)
		  	
			tombe:= update !t down b false;
			let tmp=ref (Unix.gettimeofday ())in
			let tmp'=ref (Unix.gettimeofday ())in
			while ((!tmp' -. !tmp ) < 0.3) do
				(*show !t;
			*)	tmp':=(Unix.gettimeofday ());
				let e =Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Poll] in 
				if e.Graphics.keypressed then begin
					match Graphics.read_key () with
					| '4' -> tombe:=(update !t left b false);
					| ' ' ->  while( not !tombe) do tombe:=(update !t down b false) done;
					| '6' -> tombe:=(update !t right b false );
					| '5' -> tombe:=(update !t rotate b false) ;
					| _ -> ();
				end;
			done;
			deleteAllFullLines b;
			afficheBoard b;
			
			Printf.printf("\n\n")
		done;
	    t:=!t'; 
	done;
	
	(*close_graph ()
*)



