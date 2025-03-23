from collections import deque

def find(uf, x):
    # Path compression: find the representative (root) for component x.
    if uf[x] != x:
        uf[x] = find(uf, uf[x])
    return uf[x]

def union(uf, x, y):
    # Union the components containing x and y.
    rootX = find(uf, x)
    rootY = find(uf, y)
    if rootX != rootY:
        uf[rootY] = rootX
        return True
    return False

def multi_source_bfs_mst_sum(grid, sources):
    """
    Performs a multi-source BFS on a grid and computes the sum of the edge
    weights in the resulting minimum spanning tree connecting the given sources.
    Each edge weight is calculated as the sum of the distances from the respective
    BFS origins plus 1 for the connecting step.
    
    Parameters:
      grid (List[List[int]]): 2D grid (0 for passable, 1 for obstacles)
      sources (Iterable[tuple]): List of (row, col) tuples marking starting nodes

    Returns:
      total_mst_sum (int): Total sum of the edge weights in the MST.
    """
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0

    # Mapping each cell to the original source that reached it.
    source_map = {}
    # Dictionary to track the parent cell (optional, for path reconstruction).
    cell_parent = {}
    # Distance from the respective source.
    dist = {}
    
    # Initialize union-find for the sources.
    uf = {s: s for s in sources}
    
    # Initialize the BFS queue with all source nodes.
    queue = deque()
    for s in sources:
        queue.append(s)
        source_map[s] = s   # Tag the cell with its own source.
        cell_parent[s] = None
        dist[s] = 0         # Distance is 0 for source cells.
    
    total_mst_sum = 0
    # 4-connected neighbors (up, down, left, right)
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    
    while queue:
        x, y = queue.popleft()
        for dx, dy in directions:
            nx, ny = x + dx, y + dy
            # Ensure the cell is within bounds and passable.
            if 0 <= nx < rows and 0 <= ny < cols and grid[nx][ny] == 0:
                if (nx, ny) not in source_map:
                    # Mark the neighbor with the same source and record distance.
                    source_map[(nx, ny)] = source_map[(x, y)]
                    cell_parent[(nx, ny)] = (x, y)
                    dist[(nx, ny)] = dist[(x, y)] + 1
                    queue.append((nx, ny))
                else:
                    # The neighbor is already visited.
                    source_a = source_map[(x, y)]
                    source_b = source_map[(nx, ny)]
                    # Only consider if the neighbor comes from a different source.
                    if source_a != source_b and find(uf, source_a) != find(uf, source_b):
                        # Compute edge weight: distance from (x,y) + distance from (nx,ny) + 1 for the connecting step.
                        edge_weight = dist[(x, y)] + dist[(nx, ny)] + 1
                        total_mst_sum += edge_weight
                        union(uf, source_a, source_b)
    
    return total_mst_sum

# Example usage:
if __name__ == '__main__':
    # 0 represents an open cell; 1 represents an obstacle.
    # grid = [
    #     [1, 1, 1, 1, 1, 1],
    #     [1, 0, 1, 0, 1, 1],
    #     [1, 0, 1, 0, 0, 1],
    #     [1, 0, 0, 0, 1, 1],
    #     [1, 1, 1, 1, 1, 1]
    # ]
    # grid = [
    #     [1, 1, 1, 1, 1, 1, 1],
    #     [1, 0, 0, 0, 1, 1, 1],
    #     [1, 0, 0, 0, 0, 0, 1],
    #     [1, 0, 0, 0, 1, 1, 1],
    #     [1, 0, 0, 0, 0, 0, 1],
    #     [1, 0, 0, 0, 1, 1, 1],
    #     [1, 1, 1, 1, 1, 1, 1]
    # ]
    grid = [
        [1, 1, 1, 1, 1, 1, 1],
        [1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 1],
        [1, 1, 1, 1, 1, 1, 1]
    ]



    
    # Define terminal nodes (sources) you want to connect.
    #terminals = [(1, 1), (2, 1), (3, 1), (5, 2), (2, 3), (1, 5), (2, 5), (3, 5)]
    terminals = [(1, 1), (2, 1), (1, 2), (5, 2), (4, 1), (5, 1), (2, 5), (1, 4), (1, 5), (5, 5), (4, 5), (5, 4), (3, 3)]
    
    mst_edges  = multi_source_bfs_mst_sum(grid, terminals)
    print(mst_edges)
