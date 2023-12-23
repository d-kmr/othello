

module Cell = struct
  
  type t = W | B | E

  let pp out x =
    match x with    
    | W -> Printf.fprintf out "●"
    | B -> Printf.fprintf out "○"
    | E -> Printf.fprintf out "　"
                 
  let print x = Printf.printf "%a" pp x

  let rev x =
    match x with
    | W -> B
    | B -> W
    | E -> E
end
;;

module Ban = struct

  type t = Cell.t array array (* 盤面の型 *)

  let create n : t = Array.make_matrix n n Cell.E (* n*n の空白の盤面を作る *)

  let init n = (* 初期化された盤面を作る．通常は n を偶数として使う *)
    let ban = create n in
    let k = (n-1)/2 in
    ban.(k).(k) <- Cell.W;    
    ban.(k).(k+1) <- Cell.B;
    ban.(k+1).(k) <- Cell.B;
    ban.(k+1).(k+1) <- Cell.W;
    ban

  let copy (ban:t) : t = (* ban をコピーした新規盤を返す *)
    let n = Array.length ban in
    let ban' = create n in
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        ban'.(i).(j) <- ban.(i).(j)
      done
    done;
    ban'
    
  let put (ban:t) i j stone : t = (* ban の(i,j)に石 s を単に置いた新しい盤を返す．既に石があるときは同じ盤を返す *)
    let ban' = copy ban in
    if ban'.(i).(j) = Cell.E
    then ban'.(i).(j) <- stone
    else Format.printf "The cell (%d,%d) is not empty (do nothing)\n" i j;
    ban'

  let pp_with_header header1 header2 out (ban:t) = (* 各行に文字列 header を表示しつつ盤面を表示 *)
    let n = Array.length ban in
    for i = 0 to n-1 do
      Printf.fprintf out "%s" header1;
      for j = 0 to n-1 do        
        Cell.pp out ban.(i).(j);
        Printf.fprintf out "|"
      done;
      if i = 0 then Printf.fprintf out "%s" header2;
      Printf.fprintf out "\n"
    done
    
  let println_with_header header1 header2 (ban:t) = Printf.printf "%a" (pp_with_header header1 header2) ban

  let println ban = println_with_header "" "" ban


end
;;

module Path = struct
  (* ゲーム木のノードのパス *)
  (* [] は根ノード．[0;1;3] は根から見て 0番目の子の1番目の子の3番目の子ノード *)
  type t = int list

  let pp ppf path = (* パスの整形出力．[1;2;3] を 1;2;3 へ *)
    match path with
    | [] -> Format.fprintf ppf ""
    | i::path' ->
       Format.fprintf ppf "%d" i;
       List.iter (fun j -> Format.fprintf ppf ";%d" j) path'

  let print path = Format.printf "[%a]" pp path
    
end
;;

module Color = struct
  (* 後退解析時の解析結果 *)
  (* WW: 白が必勝 *)
  (* WB: 黒が必勝 *)
  (* WE: どちらでもない *)
  (* MADA: 未解析 *)
  type t = WW | WB | WE | MADA

  let pp ppf c =
    match c with
    | WW -> Format.fprintf ppf "白必勝"
    | WB -> Format.fprintf ppf "黒必勝"
    | WE -> Format.fprintf ppf "引き分け"
    | MADA -> Format.fprintf ppf "解析前"
                 
  let print x = Format.printf "%a" pp x
              
end
;;

module GameTree = struct
  (* ゲーム木のモジュール．全体を通してゲーム木は1つしか使わない *)

  (* Nd(パス,盤面,色，親ノードへの参照,子ノードたちへの参照) *)
  type t = Nd of Path.t * Ban.t * Color.t * t ref option * t ref list (* Nd は Node の省略形 *)
(*               
  let rec get gt_ref path = (* gt 内で path の位置から始まるゲーム木を得る．該当ノードがないときは Not_found *)
    match path with
    | [] -> gt
    | i::path' ->
       try
         let Nd(_,_,_,children) = gt in
         let gt' = !(List.nth children i) in
         get gt' path'
       with
         _ ->
         let path = Format.asprintf "[%a]" Path.pp path in
         Printf.printf "Not_found: Invalid path: [%s]" path; exit 0

  int option 型なら None または Some 4 の形
  
 *)
  let get_parent (node: t ref): t ref option = 
    let Nd (_,_,_,parent,_) = !node in
    parent

  let get_children (node: t ref) = 
    let Nd (_,_,_,_,children) = !node in
    children

  let get_color (node: t ref) = 
    let Nd (_,_,color,_,_) = !node in
    color

  let get_ban (node: t ref) =
    let Nd (_,ban,_,_,_) = !node in
    ban

  let pp_node0 out (node: t ref) = (* 各行に現在位置のパスを表示しつつ node のみを表示 *)
    let Nd(path,ban,color,_,_) = !node in
    let header0 = Format.asprintf "[%a]" Path.pp path in
    let header1 = Format.asprintf " (%a)" Color.pp color in
    Printf.fprintf out "%a" (Ban.pp_with_header header0 header1) ban
    
  let rec pp_node out (node: t ref) = (* 各行に現在位置のパスを表示しつつ node から下のゲーム木を表示 *)
    let Nd(path,ban,color,_,children) = !node in
    let header0 = Format.asprintf "[%a]" Path.pp path in
    let header1 = Format.asprintf " (%a)" Color.pp color in
    Printf.fprintf out "%a" (Ban.pp_with_header header0 header1) ban;
    List.iter (Printf.fprintf out "%a" pp_node) children

  let println gt =
    let root = ref gt in
    Printf.printf "%a" pp_node root

  let write_file filename gt =
    let out_ch = open_out filename in
    let root = ref gt in
    Printf.fprintf out_ch "%a" pp_node root;
    close_out out_ch
    
end
;;

let okeru ban koma (i,j) = (*コマを置ける座標の判定*)
 if ban.(i).(j)  <> Cell.E then false
 else 
   let n = Array.length ban in
   let rec check (i,j) (x,y) =  (* 現在位置(i',j') から(x,y)方向に進んだとき，空白にあわずにkomaに到達できるかの判定 *)
     if i < 0 || j < 0 || i > n-1 || j > n-1 then false
     else if ban.(i).(j) = Cell.E then false
     else if ban.(i).(j) = koma then true
     else check (i+x,j+y) (x,y) in
   let clock (x,y) =  
     if i+x < 0 || j+y < 0 || i+x > n-1 || j+y > n-1 then false
     else if ban.(i+x).(j+y) = Cell.E then false
     else if ban.(i+x).(j+y) = koma then false
     else check (i+2*x, j+2*y) (x,y) in
   if  clock (-1,-1) || clock (-1,0) || clock (-1,1) || clock (0,-1) ||
      clock (0,1) || clock (1,-1) || clock (1,0) || clock (1,1) then true
   else false
;;

let okeruall ban koma : (int * int) list = (*コマが置ける座標の列挙*)
  let n = Array.length ban in
  let za = ref [] in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if okeru ban koma (i,j) then ((* Format.printf "(%d,%d)\n" i j; *) za:=(i,j)::!za)
      else ()
      done
  done;
  !za 
;;

let change ban koma (i,j) :Ban.t = (* 盤面(ban)の座標(i,j)にコマ(koma)を置いたときに，次の盤面を作る *)
  let ban' = Ban.copy ban in
    ban'.(i).(j) <- koma ;
    let n = Array.length ban in
    let rec rev (i',j') (x,y) = (* 現在位置(i',j')から(x,y)方向にkomaに到達するまで石をひっくり返す*)
      if ban'.(i').(j') = koma then ()  
      else ( ban'.(i').(j') <-koma; rev(i'+x,j'+y) (x,y) )
    in
    let rec watch (i',j') (x,y)  = (* 現在位置(i',j') から(x,y)方向に進んだとき，空白にあわずにkomaに到達できるかの判定 *)
      if i'+ x < 0 || j'+ y < 0 || i'+ x > n-1 || j'+ y > n-1 then false 
      else if ban'.(i'+ x).(j'+ y) = Cell.E then false
      else if ban'.(i'+ x).(j'+ y) = koma then true
      else watch (i'+ x,j'+ y) (x,y)
    in
    let reverse (x,y) = (* 置きたい場所(i,j)から(x,y)方向に石が返せるなら石をひっくり返す *)
      if i+ x < 0 || j+ y < 0 || i+ x > n-1 || j+ y > n-1 then () 
      else if ban'.(i+x).(j+y) = koma || ban'.(i+x).(j+y) = Cell.E then ()
      else if watch (i,j) (x,y) = false then ()
      else rev (i+x,j+y) (x,y)
    in 
    reverse (-1,-1) ; reverse (-1,0) ;  reverse (-1,1) ; reverse (0,-1) ;
    reverse (0,1) ; reverse (1,-1) ; reverse (1,0) ; reverse (1,-1)  ;
    ban'
;;      



let count ban = (* 盤面(ban)のkoma(黒，白)の数を数える *) 
  let n = Array.length ban in
  let co = ref 0 in
  let eo = ref 0 in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if ban.(i).(j) = Cell.W then co:= !co + 1
      else if ban.(i).(j) = Cell.B then eo:= !eo + 1
      else ()  
    done
  done;
 (!co ,!eo)
;;

let shouhai ban = (*勝敗がついた盤面(ban)を見て，白必勝か，黒必勝か，引き分けのいずれかを求める *)
  (* let n = Array.length ban in *)
  let (co , eo) = count ban in
  if co > eo then Color.WW
  else if co < eo then Color.WB
  else Color.WE
;;

let nodelist : GameTree.t ref list ref = ref [] (* 次に後退解析をする盤面のリスト *)
;;

let gtcount = ref 0 (* ゲーム木のノード数のカウント *)
;;

let rec makeGameTree parent koma (ban : Ban.t) path : GameTree.t ref =  (* ゲーム木をつくる関数 *)
  (* 1,入力されたban(盤面)とkoma(コマ)に対してokeruall ban koma を使い koma(コマ) が置ける座標を求める*)
  let sa =  okeruall ban koma in
  (* 2,1で得た座標のリストを使いchange ban koma (i,j) を実行する *)
  let sb : Ban.t list = List.map (change ban koma) sa in
  let children = ref [] in
  let thisnode = ref (GameTree.Nd(path,ban,Color.MADA,parent,[])) in
  for i = 0 to List.length sb - 1 do
    (* Ban.println (List.nth sb i); *)
    children := (makeGameTree (Some thisnode) (Cell.rev koma) (List.nth sb i) (path @ [i])) :: !children
  done;
  thisnode := GameTree.Nd(path,ban,Color.MADA,parent,List.rev !children);
  if !children = [] then nodelist := thisnode :: !nodelist;
  gtcount := !gtcount + 1;
  thisnode
;;  
 let nodecolor node =
   let GameTree.Nd(_,_,color,_,_)  = !node in
   color
 ;;

 let nodechildren node =
   let GameTree.Nd(_,_,_,_,children) = !node in
   children
 ;;

 let nodepath node =
   let GameTree.Nd(path,_,_,_,_) = !node in
   path
 
 let awq node =
   let children = nodechildren node in
   List.map nodecolor children
 ;;
(*
 let goodparent (node :GameTree.t) : GameTree.t ref option   =
   let GameTree.Nd(_,_,_,parent,_) = !node in
   parent *)
 ;;

 let p = fun c -> c = Color. WW
 let pp = fun c -> c = Color. WB
 let ppp = fun c -> c = Color. WE
 ;;

 let owl node =
    let ok = awq node in
    if List.exists p ok then Color.WW
    else if (List.exists pp ok) && (List.exists ppp ok) then Color.WE
    else if List.for_all pp ok then Color.WB
    else Color.MADA
 ;;

 let owll node =
    let ok = awq node in
    if List.exists pp ok then Color.WB
    else if (List.exists p ok) && (List.exists ppp ok) then Color.WE
    else if List.for_all p ok then Color.WW
    else Color.MADA

 ;;
let ironuri node =
    if (List.length (nodepath node) - 1 ) mod 2 = 1 then owl node
    else owll node
;;

(* node の親ノードが nodelist に含まれているかをチェックする関数 *)
(* 親ノードがない場合：false *)
(* 親ノードがある場合：親がnodelistに含まれていたら true そうでなければ false *)
let rec ppap node (nodelist: GameTree.t ref list ref) : bool =
  match GameTree.get_parent node with
  | None -> false
  | Some parent -> List.mem parent !nodelist
(*
  match nodelist with
    [] -> false
  | first :: rest -> if first = parentnode  then true
                     else ppap node rest
 *)
;;

(* 後退解析 *)
(* nodelist の中の要素を1つずつ見て色をつける *)
(* 色をつけたらその親を探して nodelist に追加する *)
(* 色がつかなかったら nodelist の最後に回す *)
let retrograde (gt: GameTree.t) =
  (* (1) 葉に色をつける *)
  Format.printf "@[Stage 1 (葉に色をつける) 開始@.";
  let newnodelist : GameTree.t ref list ref = ref [] in
  for i = 0 to List.length !nodelist - 1 do
    let leafnode = List.nth !nodelist i in  
    let GameTree.Nd(path,ban,_,parent,_) = !leafnode in
    let color = shouhai ban in
    leafnode := Nd(path,ban,color,parent,[]);
    match parent with
    | None -> ()
    | Some parentnode -> newnodelist := parentnode :: !newnodelist
  done;
  Format.printf "@[Stage 1 終了@.";  
  Format.printf "@[Next nodes: %d@." (List.length !newnodelist);
  nodelist := !newnodelist;
  
  (* (2) 葉でないノードに色をつける *)
  Format.printf "@[Stage 2 (葉から上に色をつける) 開始@.";
  let finished = ref 0 in
  while !nodelist <> []        
  do
    Format.printf "@[[%d/%d] |Nodelist|=%d@." !finished !gtcount (List.length !nodelist);
    let node = List.hd !nodelist in
    nodelist := List.tl !nodelist; (* 先頭の1つをとり除く *)
    let GameTree.Nd(_,_,_,parent,_) = !node in
    let color = ironuri node in
    if color = Color.MADA then nodelist := !nodelist @ [node]
    else finished := !finished + 1;
    
    match GameTree.get_parent node with
    | None -> ()
    | Some parentnode ->
       if List.mem parentnode !nodelist then ()
       else nodelist := parentnode :: !nodelist
      ;
        
  done;
  Format.printf "@[Stage 2 終了@.";  
  Format.printf "@[ファイルに書き出し: result.txt@.";  
  GameTree.write_file "result.txt" gt
;;

let () =

  let open Cell in (* 毎回 Cell と書きたくないので省略できるようにする *)
(*  
  let ban0 = Ban.init 4 in (* 4*4 の初期化された盤面 ban を生成 *)
  let ban1 = Ban.put ban0 0 1 W in (* ban1 は ban0 の(0,1)に白を置いた盤面 *)
  let ban2 = Ban.put ban0 3 3 B in (* ban2 は ban0 の(3,3)に黒を置いた盤面 *)
  let _otya =  okeruall ban0 W in (*コマが置けるマスの列挙*)
  Ban.println ban0; (* ban0 を表示 *)
  Format.printf "\n"; (* 空白行を空ける *)
  let _otya2 =  okeruall ban1 W in (*コマが置けるマスの列挙*)
  Ban.println ban1; (* ban1 を表示 *)
  Format.printf "\n"; (* 空白行を空ける *)
  let _otya3 =  okeruall ban2 W in (*コマが置けるマスの列挙*)
  Ban.println ban2; (* ban2 を表示 *)
  Format.printf "\n";
  let _otya4 = change ban2 W (0,2) in (* オセロ盤面2において，座標(0,2)に白を置いたときに盤面を作る *)
  Ban.println ban2;
 *)
  Printf.printf "ゲーム木を生成開始\n";
  let gt : GameTree.t = !(makeGameTree None Cell.W (Ban.init 4) []) in
  Printf.printf "ゲーム木を生成終了 (サイズ：%d)\n" !gtcount;
  (* Printf.printf "Leaf nodes: %d\n" (List.length !nodelist);  *)
  (* GameTree.println gt *)
  Format.printf "@[後退解析開始@.";    
  retrograde gt;
  Format.printf "@[後退解析終了@.";
                  
  (*
  Printf.printf "次に解析する盤面\n";
  List.iter (Printf.printf "%a" GameTree.pp_node0) !nodelist
   *)
    
(*  
  let gt = GameTree.init 4 in
  GameTree.addban gt [] ban1; (* [0]にban1を追加 *)
  GameTree.addban gt [] ban2; (* [1]にban2を追加 *)
  GameTree.addban gt [0] ban1; (* [0;0]にban1を追加 *)
  GameTree.addban gt [0] ban2; (* [0;1]にban2を追加 *)

  GameTree.println gt ;
 *)

  



