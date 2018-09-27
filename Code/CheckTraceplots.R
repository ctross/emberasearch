set.seed(1)
GGt_m1<-traceplot(m1,pars=sample(names(m1@sim$samples[[1]]),30))
ggsave("TraceM1.pdf",GGt_m1,width=11,height=8.5)

GGt_m2<-traceplot(m2,pars=sample(names(m2@sim$samples[[1]]),30))
ggsave("TraceM2.pdf",GGt_m2,width=11,height=8.5)

GGt_m3<-traceplot(m3,pars=sample(names(m3@sim$samples[[1]]),30))
ggsave("TraceM3.pdf",GGt_m3,width=11,height=8.5)

