module InGraph 

    type INGraph<'a> =  'a list * IEdge list 
    and IEdge = int list

    let make_graph vertices =
        let vertices = List.map (fun v -> v) vertices
        let adjacent = List.map (fun v -> List.empty<int>) vertices
        (vertices, adjacent)

    let add_vertice vertice aGraph =
        let vertices , adjacent =  aGraph
        let r = List.contains vertice vertices
        let vertices = if not r then vertices@[vertice] else vertices
        let idx = List.findIndex (fun e -> e = vertice) vertices
        let x = List.tryItem idx adjacent
        let adjacent = 
            match x with
            | Some(_) -> List.insertAt idx (List.empty<int>) adjacent
            | None -> []
        (vertices, adjacent)

    let get_vertice index aGraph = 
        List.item index (fst aGraph)

    let add_edge verticeA verticeB aGraph =
        let vertices, adjacent = aGraph
        let rA = List.contains verticeA vertices
        let rB = List.contains verticeB vertices
        
        let vertices = if not rA then vertices@[verticeA] else vertices
        let vertices = if not rB then vertices@[verticeB] else vertices
        
        let iA = List.findIndex (fun e -> e = verticeA) vertices
        let iB = List.findIndex (fun e -> e = verticeB) vertices

        let xA = List.tryItem iA adjacent
         

        
        let adjacent = if not rA then adjacent@[verticeA] else adjacent
        let adjacent = if not rB then adjacent@[verticeB] else adjacent
        
        printfn "[add_edge] indexA = %A, indexB = %A" iA iB
        let neighbors = List.item iA adjacent
        let nAdjacent = (List.removeAt iA adjacent)@[iB::neighbors]
        (vertices, nAdjacent)
        