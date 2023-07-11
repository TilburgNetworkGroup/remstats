#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// Function to remove duplicate slices from a three-dimensional array
arma::cube remove_duplicate_slices(const arma::cube& input_array) {
    int n_slices = input_array.n_slices;
    int n_rows = input_array.n_rows;
    int n_cols = input_array.n_cols;

    // Create a boolean vector to track the unique slices
    std::vector<bool> keep_slice(n_slices, true);

    // Check for duplicate slices
    for (int i = 0; i < n_slices; i++) {
        if (keep_slice[i]) {
            for (int j = i + 1; j < n_slices; j++) {
                if (arma::all(arma::vectorise(input_array.subcube(0, 0, i, n_rows - 1, n_cols - 1, i) ==
                                            input_array.subcube(0, 0, j, n_rows - 1, n_cols - 1, j)))) {
                    Rcpp::warning("Duplicate statistics detected. Removing the duplicate slice.");
                    keep_slice[j] = false;
                }
            }
        }
    }

    // Count the number of unique slices
    int unique_slices = std::count(keep_slice.begin(), keep_slice.end(), true);

    // Create the output array with unique slices
    arma::cube output_array(n_rows, n_cols, unique_slices);

    // Copy the unique slices to the output array
    int current_index = 0;
    for (int i = 0; i < n_slices; i++) {
        if (keep_slice[i]) {
            output_array.slice(current_index) = input_array.slice(i);
            current_index++;
        }
    }

    return output_array;
}


// Function to combine statistic arrays. The function is set up to combine 
// along any dimension (1, 2, or 3). 
//[[Rcpp::export]]
arma::cube combine_arrays(const Rcpp::List& array_list, int along) {
    // Check if the list is empty
    if (array_list.size() == 0) {
        Rcpp::stop("Input list is empty.");
    }

    // Check if along is valid
    if (along < 1 || along > 3) {
        Rcpp::stop("Invalid along. Must be 1, 2, or 3.");
    }

    // Get the first array to determine the dimensions
    arma::cube first_array = array_list[0];
    arma::uword n_rows = first_array.n_rows;
    arma::uword n_cols = first_array.n_cols;
    arma::uword n_slices = first_array.n_slices;

    if (along == 1) {
        // Calculate the total number of rows
        arma::uword total_rows = 0;
        
        for (int i = 0; i < array_list.size(); i++) {
            arma::cube current_array = array_list[i];
            
            // Check if dimensions match
            if (current_array.n_cols != n_cols || current_array.n_slices != n_slices) {
            Rcpp::stop("Arrays must have the same number of columns and slices.");
            }
            
            total_rows += current_array.n_rows;
        }

        n_rows = total_rows;

    } else if (along == 2) {
        // Calculate the total number of columns
        arma::uword total_cols = 0;
        
        for (int i = 0; i < array_list.size(); i++) {
            arma::cube current_array = array_list[i];
            
            // Check if dimensions match
            if (current_array.n_rows != n_rows || current_array.n_slices != n_slices) {
            Rcpp::stop("Arrays must have the same number of rows and slices.");
            }
            
            total_cols += current_array.n_cols;
        }

        n_cols = total_cols;

    } else if (along == 3) {
        // Calculate the total number of slices
        arma::uword total_slices = 0;
        
        for (int i = 0; i < array_list.size(); i++) {
            arma::cube current_array = array_list[i];
            
            // Check if dimensions match
            if (current_array.n_rows != n_rows || current_array.n_cols != n_cols) {
            Rcpp::stop("Arrays must have the same number of rows and columns.");
            }
            
            total_slices += current_array.n_slices;
        }

        n_slices = total_slices;
    }

    // Create the combined array
    arma::cube combined_array(n_rows, n_cols, n_slices);

    if (along == 1) {
        // Copy data from each array in the list
        arma::uword current_row = 0;
        
        for (int i = 0; i < array_list.size(); i++) {
            arma::cube current_array = array_list[i];
            arma::uword current_rows = current_array.n_rows;
            
            // Copy data to combined array
            combined_array.subcube(current_row, 0, 0, current_row + current_rows - 1, n_cols - 1, n_slices - 1) = current_array;
            
            current_row += current_rows;
        }

    } else if (along == 2) {
        // Copy data from each array in the list
        arma::uword current_col = 0;
        
        for (int i = 0; i < array_list.size(); i++) {
            arma::cube current_array = array_list[i];
            arma::uword current_cols = current_array.n_cols;
            
            // Copy data to combined array
            combined_array.subcube(0, current_col, 0, n_rows - 1, current_col + current_cols - 1, n_slices - 1) = current_array;
            
            current_col += current_cols;
        }

    } else if (along == 3) {
        // Copy data from each array in the list
        arma::uword current_slice = 0;
            
        for (int i = 0; i < array_list.size(); i++) {
            arma::cube current_array = array_list[i];
            arma::uword current_slices = current_array.n_slices;
                
            // Copy data to combined array
            combined_array.slices(current_slice, current_slice + current_slices - 1) = current_array;
                
            current_slice += current_slices;
        }

    }

    // Remove duplicate slices
    combined_array = remove_duplicate_slices(combined_array);
       
    return combined_array;      
}


