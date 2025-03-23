use std::{
    io::{self, Read},
};

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



        let mut dij = || {
            for k in 0..n {
                for i in 0..n {
                    for j in 0..n {
                        edge_mat[i][j] = edge_mat[i][j].min(edge_mat[i][k] + edge_mat[k][j]);
                        if edge_mat[i][j] > edge_mat[i][k] + edge_mat[k][j]
                        {
                            edge_mat[i][j] = -f64::INFINITY;
                        }
                    }
                }
            }
        };

        dij();




        for _ in 0..q {
            let u = next() as usize;
            let v = next() as usize;
            if edge_mat[u][v] == f64::INFINITY {
                result.push_str( "Impossible\n");
            }
            else if edge_mat[u][v] == -f64::INFINITY {
                result.push_str( "-Infinity\n");
            }
            else {
                result.push_str( &(edge_mat[u][v].to_string() + "\n"));
            }
        }
        result.push_str( "\n");
        
    }

    print!("{}", result);
}
