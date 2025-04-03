use std::io::{self, Read};

/*
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    // let file_path = "allpairspath.in";
    // let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut content = String::new();
    io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();
    let mut next = || -> i64 { tokens.next().unwrap().parse().unwrap() };

    let mut result = String::new();

    loop {
        let n = next() as usize;
        let m = next() as usize;
        let q = next() as usize;

        if n == 0 && m == 0 && q == 0 {
            break;
        }

        let mut edge_mat: Vec<Vec<f64>> = vec![vec![f64::INFINITY; n]; n];

        for i in 0..n {
            edge_mat[i][i] = 0.0
        }

        for _ in 0..m {
            let u = next() as usize;
            let v = next() as usize;
            let c = next() as f64;
            edge_mat[u][v] = edge_mat[u][v].min(c);
        }

        let edge_mat = floyd_warshall(edge_mat);

        // Read the resulting edge_mat
        for _ in 0..q {
            let u = next() as usize;
            let v = next() as usize;
            if edge_mat[u][v] == f64::INFINITY {
                result.push_str("Impossible\n");
            } else if edge_mat[u][v] == -f64::INFINITY {
                result.push_str("-Infinity\n");
            } else {
                result.push_str(&(edge_mat[u][v].to_string() + "\n"));
            }
        }
        result.push_str("\n");
    }

    print!("{}", result);
}

/*
 * This is a simple implmentation of the floyd warshall implementation.
 * This algoritm works simply works by comparing the cost of every edge with the cost of taking a
 * path with an intermediet node k. Then simply
 * picking the smaller of the two. Doing this over the whole of the graph will produce the best
 * cost of going from some node some to some other node.
 *
 * There is one extra check to see if the node is located in a infinit negative loop.
 * If it is the case that a path going from i to i and we can take some detour to k that produces
 * some cost that is lower after taking that detour twice we know that there is some negative
 * cyckle and we mark that node as negative infinity.
 *
 * Input/Output:
 * The input of this function is a edge matrix some position in the matrix e_(i,j) denotes the cost
 * of going from node i to j. Where we mark the lake of a edge between node with infinit cost
 *
 * The output of this function will be a distance matrix denoting the cost of going fomr some node
 * to some other node.
 *
 * Complexity:
 * The complexity of this function will simply be the derived from the number of for loops.
 * As we have three for loop and each of them will go over all node.
 * We can surmize that the complexity of the function will be O(n^3) where n is the number of
 * nodes.
 *
 * */
fn floyd_warshall(mut edge_mat: Vec<Vec<f64>>) -> Vec<Vec<f64>> {
    let n = edge_mat.len();
    for k in 0..n {
        for i in 0..n {
            for j in 0..n {
                edge_mat[i][j] = edge_mat[i][j].min(edge_mat[i][k] + edge_mat[k][j]);
                if edge_mat[i][j] > edge_mat[i][k] + edge_mat[k][j]
                // Check for negative cyckle
                {
                    edge_mat[i][j] = -f64::INFINITY;
                }
            }
        }
    }
    return edge_mat;
}
