module ALGraph

    let makeGraph (vertices: 'a list) =
        Map (List.map (fun v -> (v, Set.empty<'a>)) vertices)

    let addVertice vertice aGraph =
        if not (Map.containsKey vertice aGraph) then
            Map.add vertice Set.empty aGraph
        else
            aGraph

    let removeVertice vertice aGraph =
        // ALGO:
        // find all entrances of the vertice in adjacent lists
        // remove all entrances of the vertice
        // remove the vertice itself
        let removeAV k v =
            if Set.contains vertice v then
                Set.remove vertice v
            else
                v
        let nMap = Map.map removeAV aGraph
        Map.remove vertice nMap

    let addEdge verticeA verticeB aGraph =
        // ALGO:
        // check existance of the vertices in the graph
        // there are 4 cases
        // ~E(A), E(B) -> add vertice A, and then do the narmal procedure
        // E(A), ~E(B) -> add vertice B, and then do the narmal procedure
        // ~E(A), ~(B) -> add vertice A, add vertice B and then do the narmal procedure
        // E(A), E(B) -> a normal procedure: add B to adjacent list of A, add A to adjacent list of B
        let nGraph = 
            if not (Map.containsKey verticeA aGraph) then
                Map.add verticeA (Set.singleton verticeB) aGraph
            else 
                aGraph
        let nGraph = 
            if not (Map.containsKey verticeB nGraph) then
                Map.add verticeA (Set.singleton verticeA) nGraph
            else 
                nGraph
        let modifyAL vertice x = 
            match x with 
            | Some(al) -> Some(Set.add vertice al) 
            | None -> None
        let nGraph = Map.change verticeA (modifyAL verticeB) nGraph
        let nGraph = Map.change verticeB (modifyAL verticeA) nGraph
        nGraph

    let removeEdge verticeA verticeB aGraph =
        let modifyAL vertice x = 
            match x with 
            | Some(al) -> Some(Set.remove vertice al) 
            | None -> None
        let nGraph = Map.change verticeA (modifyAL verticeB) aGraph
        let nGraph = Map.change verticeB (modifyAL verticeA) nGraph
        nGraph

    let adjacents vertice aGraph =
        let result = Map.tryFind vertice aGraph
        match result with 
        | Some(adjacents) -> adjacents
        | None -> Set.empty

    