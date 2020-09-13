theory Locales 
  imports Main 
begin 

locale Carrier =  
  fixes A 

locale AssocPM = Carrier + 
  fixes op (infix "." 70) and e 
  assumes "(x . (y . z)) \<equiv> ((x . y) . z)" 
      and "e . x \<equiv> x"
      and "x . e \<equiv> x" 

locale CommPM = Carrier + 
  fixes op' (infix "$" 70) and e 
  assumes "x $ y \<equiv> y $ x"
      and "e $ x \<equiv> x"
      and "x $ e \<equiv> x" 


locale Assoc2 = AssocPM A om e for A om e
locale Comm2 = CommPM A om d for A om d 

locale Test = AssocPM + CommPM 

locale Test2 = Assoc2 + Comm2 