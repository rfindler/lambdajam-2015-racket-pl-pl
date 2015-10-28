#lang mini-hdl

inputs a5,a4,a3,a2,a1,a0=11;
inputs b5,b4,b3,b2,b1,b0=22;

s0 = a0 ⊕ b0;
c0 = a0 ∧ b0;

s1 = a1 ⊕ b1 ⊕ c0;
c1 = (a1 ∧ b1) ∨ (c0 ∧ (a1 ⊕ b1));

s2 = a2 ⊕ b2 ⊕ c1;
c2 = (a2 ∧ b2) ∨ (c1 ∧ (a2 ⊕ b2));

s3 = a3 ⊕ b3 ⊕ c2;
c3 = (a3 ∧ b3) ∨ (c2 ∧ (a3 ⊕ b3));

s4 = a4 ⊕ b4 ⊕ c3;
c4 = (a4 ∧ b4) ∨ (c3 ∧ (a4 ⊕ b4));

s5 = a5 ⊕ b5 ⊕ c4;
c5 = (a5 ∧ b5) ∨ (c4 ∧ (a5 ⊕ b5));

showint(c5,s5,s4,s3,s2,s1,s0);
