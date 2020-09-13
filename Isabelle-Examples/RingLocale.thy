theory RingLocale 
  imports Main 
begin 

typedecl A 

locale monoid = 
  fixes op :: "A \<Rightarrow> A \<Rightarrow> A" (infix "." 70) and
        e :: "A" 
  assumes "x . (y . z) \<equiv> (x . y) . z" 
      and "e . x \<equiv> x"
      and "x . e \<equiv> x" 

consts op :: "A \<Rightarrow> A \<Rightarrow> A" (infix "\<otimes>" 70)

locale monoid2 = 
  fixes G (structure) 
  assumes "x \<in> carrier G \<Longrightarrow> e \<otimes> x \<equiv> x"

definition
  Units :: "_ => 'a set"
  \<comment> \<open>The set of invertible elements\<close>
  where "Units G = {y\<cdot> y \<in> carrier G \<and> (\<exists>x \<in> carrier G\<cdot> x \<otimes> y = 1 \<and> y \<otimes> x = 1}"  

locale group = monoid2 +
  assumes "carrier G <= Units G"

