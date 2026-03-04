#include "../src/quartanion.h"
#include <iostream>

#include "lib.h"

int main()
{
    quartanion<double> q(1, 2, 3, 4);
    quartanion<double> r(5, 6, 7, 8);

    std::cout << "------ Conjugate Rules --------" << std::endl;
    auto q_conj = q.conjugate();
    auto q_conj_conj = q_conj.conjugate();
    std::cout << "q:           " << q << std::endl;
    std::cout << "q_conj:      " << q_conj << std::endl;
    std::cout << "q_conj_conj: " << q_conj_conj << std::endl;
    assert_equal(q, q_conj_conj, "q should equal its double conjugate");

    std::cout << std::endl;
    auto q_plus_r = q + r;
    auto q_plus_r_conj = q_plus_r.conjugate();
    auto q_conj_plus_r_conj = q_conj + r.conjugate();
    assert_equal(q_plus_r_conj, q_conj_plus_r_conj, "(q + r)* = q* + r*");

    std::cout << std::endl;
    auto q_times_r = q * r;
    auto q_times_r_conj = q_times_r.conjugate();
    auto r_conj_times_q_conj = r.conjugate() * q.conjugate();
    assert_equal(q_times_r_conj, r_conj_times_q_conj, "(q * r)* = r* * q*");

    std::cout << "------ Norm Rules --------" << std::endl;
    auto q_norm = q.norm();
    auto q_conj_norm = q_conj.norm();
    std::cout << "q norm:      " << q_norm << std::endl;
    std::cout << "q_conj norm: " << q_conj_norm << std::endl;
    assert_double_equal(q_norm, q_conj_norm, "n(q) = n(q*)");

    // n(qr) = n(q) * n(r)
    auto qr_norm = q_times_r.norm();
    auto q_norm_times_r_norm = q_norm * r.norm();
    assert_double_equal(qr_norm, q_norm_times_r_norm, "n(qr) = n(q) * n(r)");

    std::cout << "------ Linear Rules --------" << std::endl;
    // p(sq + tr) = spq + trp
    double s = 2.0, t = 3.0;
    auto p = quartanion<double>(9, 10, 11, 12);
    auto sq_plus_tr = s * q + t * r;
    auto p_times_sq_plus_tr = p * sq_plus_tr;
    auto spq_plus_trp = s * (p * q) + t * (p * r);
    assert_equal(p_times_sq_plus_tr, spq_plus_trp, "p(sq + tr) = spq + trp");

    // (sq + tr)p = spq + trp
    auto sq_plus_tr_times_p = sq_plus_tr * p;
    auto spq_plus_trp_right = s * (q * p) + t * (r * p);
    assert_equal(sq_plus_tr_times_p, spq_plus_trp_right, "(sq + tr)p = spq + trp");
}