#lang mini-hdl
inputs a1,a0 = 2;
inputs b1,b0 = 1;
s0 = a0 ⊕ b0;
c0 = a0 ∧ b0;
s1 = a1 ⊕ b1 ⊕ c0;
c1 = (a1 ∧ b1) ∨
     (c0 ∧ (a1 ⊕ b1));
showint(c1,s1,s0);
