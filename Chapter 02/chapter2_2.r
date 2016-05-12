library(gRain)


F = cptable(~F, values=c(10,90),levels=val)
C = cptable(~C|F, values=c(10,90,20,80),levels=val)
E = cptable(~E|F, values=c(50,50,30,70),levels=val)
A = cptable(~A|C, values=c(50,50,70,30),levels=val)
D = cptable(~D|E, values=c(60,40,70,30),levels=val)
B = cptable(~B|A:D, values=c(60,40,70,30,20,80,10,90),levels=val)

plist = compileCPT(list(F,E,C,A,D,B))
plist

print(plist$F)
print(plist$B)

jtree = grain(plist)
jtree

querygrain(jtree, nodes=c("F"), type="marginal")
querygrain(jtree, nodes=c("C"), type="marginal")
querygrain(jtree, nodes=c("B"), type="marginal")
querygrain(jtree, nodes=c("A","B"), type="joint")
querygrain(jtree, nodes=c("A","B","C"), type="joint")

jtree2 = setEvidence(jtree, evidence=list(F="true"))
querygrain(jtree, nodes=c("F"), type="marginal")
querygrain(jtree2, nodes=c("F"), type="marginal")
querygrain(jtree, nodes=c("A"), type="marginal")
querygrain(jtree2, nodes=c("A"), type="marginal")
querygrain(jtree, nodes=c("B"), type="marginal")
querygrain(jtree2, nodes=c("B"), type="marginal")

jtree3 = setEvidence(jtree, evidence=list(F="true",A="false"))
querygrain(jtree, nodes=c("C"), type="marginal")
querygrain(jtree2, nodes=c("C"), type="marginal")
querygrain(jtree3, nodes=c("C"), type="marginal")

