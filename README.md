# i_charge
1. To compile using gnu compiler.

   gfortran -w -O3            read_qcube.f
                              make_input.f      read_input.f
                            build_matrix.f    const_matrix.f
                                  lubksb.f          ludcmp.f
                                  svbksb.f          svdcmp.f
                               solve_lud.f       solve_svd.f
                            print_matrix.f    print_charge.f
                                  pythag.f  charge_fitting.f
            -o  i_CHARGE

           << OR >>

gfortran -w -O3 set_name.f set_radius.f read_qcube.f make_input.f read_input.f build_matrix.f const_matrix.f lubksb.f ludcmp.f svbksb.f svdcmp.f solve_lud.f solve_svd.f print_matrix.f print_charge.f pythag.f charge_fitting.f -o i_CHARGE

   a) -std=legacy or -w to surpress the warnings
   b) -O3

2. The charge_fitting.f subroutine is the main driver for fitting charges.

3. The read_qcube.f subroutine reads electrostatic potential file, a qube file,
   generated from QM calculations.

4. The write_matrix.f subroutine creates A and b matrices.

5. The LU decomposition subroutine includes three extra subroutines
   from numerical recepies. It decomposes A matrix to L and U matrices.

6. The SVD decomposition subroutine includes two extra subroutines
   that are also taken from numerical recepies. It decomposes A matrix
   to U, W, and V matrices.
