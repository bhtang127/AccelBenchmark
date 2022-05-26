######################################################################################
## Codes reproducing the experiments

# library(AccelBenchmark)
#
# ####################################################################################
# ## Poisson Mixture
# proj_poi = function(x){
#   x[1] = max(min(x[1], 0.99), 0.01)
#   x[2:3] = pmax(x[2:3], 0.01)
#   x
# }

# qn_control = list(qn=2)

# set.seed(54321)
# res_poi = benchmark(
#   "poissmix",
#   algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
#   ntimes=500,
#   control = list(maxiter=3000, projection=proj_poi, tol=1e-7),
#   control.spec = list(qn=qn_control)
# )

# summary(res_poi, rate_tol=0.01)


# #####################################################################################
# ## mvt mmf
# # qn_control = list(qn=5)
# # daarem_control = list(order=10)
# # sqem_control = list(method=3)
# # control.spec = list(squarem=sqem_control,
# #                     daarem=daarem_control,
# #                     qn=qn_control)
# proj_mvt = function(x){
#   x[1] = max(x[1], 1)
#   if(x[4] * x[7] < x[5] * x[6]){
#     x[c(4, 7)] = pmax(x[c(4, 7)], sqrt(abs(x[5] * x[6])) + 1e-5)
#   }
#   x
# }

# set.seed(54321)
# res_mmf_df3 = benchmark(
#   "mvt_mmf",
#   algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
#   ntimes=200,
#   control = list(maxiter=2000, projection=proj_mvt, tol=1e-7),
#   df = 3
# )

# plot(res_mmf_df3, start_from=2)

# summary(res_mmf_df3, rate_tol=0.3)

# ###
# set.seed(54321)
# res_mmf_df10 = benchmark(
#   "mvt_mmf",
#   algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
#   ntimes=200,
#   control = list(maxiter=2000, projection=proj_mvt, tol=1e-7),
#   df = 10
# )

# plot(res_mmf_df10, start_from=2)

# summary(res_mmf_df10, rate_tol=0.3)

# ###
# set.seed(54321)
# res_mmf_df25 = benchmark(
#   "mvt_mmf",
#   algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
#   ntimes=200,
#   control = list(maxiter=2000, projection=proj_mvt, tol=1e-7),
#   df = 25
# )

# summary(res_mmf_df25, rate_tol=0.5)


# ###########################################################################################
# ## lasso lam 10
# sqem_ctrl = list(step.min0=0)
# qn_control = list(qn=5)

# set.seed(54321)
# res_l10 = benchmark(
#   "lasso",
#   algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
#   ntimes = 200,
#   control = list(maxiter=20000),
#   control.spec = list(squarem=sqem_ctrl, qn=qn_control),
#   lam = 10
# )

# plot(res_l10)

# summary(res_l10, rate_tol=0.01)


# ###########################################################################################
# ## lasso lam 1
# set.seed(54321)
# res_l1 = benchmark(
#   "lasso",
#   algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
#   ntimes = 200,
#   control = list(maxiter=20000),
#   control.spec = list(squarem=sqem_ctrl, qn=qn_control),
#   lam = 1
# )

# plot(res_l1)

# summary(res_l1, rate_tol=0.01)


# ###########################################################################################
# ## lasso lam 0.1
# set.seed(54321)
# res_l01 = benchmark(
#   "lasso",
#   algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
#   ntimes = 200,
#   control = list(maxiter=20000),
#   control.spec = list(squarem=sqem_ctrl, qn=qn_control),
#   lam = 0.1
# )

# summary(res_l01, rate_tol=0.01)


# ###########################################################################
# ## varbvs combination
# ###########################################################################
# sqem_ctrl = list(step.min0=0, method=3)
# qn_control = list(qn=5)

# set.seed(54321)
# res_bvs_50 = benchmark(
#   "bvs",
#   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#   ntimes = 200,
#   control = list(maxiter=1000, par.track=FALSE,
#                  convtype="parameter", tol=1e-7,
#                  projection=varbvs.proj(2000)),
#   control.spec = list(squarem=sqem_ctrl, qn_v3=qn_control),
#   n=200, p=2000, sd=1e-1, rate=0.05,
#   sample_every_time=F
# )

# summary(res_bvs_50, 0.01)


# set.seed(54321)
# res_bvs_100 = benchmark(
#   "bvs",
#   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#   ntimes = 200,
#   control = list(maxiter=1000, par.track=FALSE,
#                  convtype="parameter", tol=1e-7,
#                  projection=varbvs.proj(2000)),
#   control.spec = list(squarem=sqem_ctrl, qn=qn_control),
#   n=200, p=2000, sd=1e-1, rate=0.1,
#   sample_every_time=F
# )

# summary(res_bvs_100, rate_tol=0.01)


# ###########################################################################
# ## t-SNE
# ###########################################################################
# sqem_ctrl = list(step.min0=0, method=3)
# qn_control = list(qn=5)

# set.seed(54321)

# res_tsne = benchmark(
#   "tsne",
#   algorithm=c("raw", "daarem", "squarem", "nes", "qn", "pem"),
#   ntimes = 50,
#   control = list(maxiter=1000, convtype="objfn", tol=1e-4),
#   control.spec = list(squarem=sqem_ctrl, qn=qn_control),
#   perplexity=30,
#   rho = 5e-6,
#   sd = 5e-3,
#   sample_every_time = FALSE
# )

# summary(res_tsne, loss_tol=0.3)


# ##############################################################################
# ## Sinkhorn
# ##############################################################################
# set.seed(54321)
# res_m3 = benchmark(
#   "sinkhorn",
#   algorithm=c("raw", "daarem", "squarem", "nes", "qn", "pem"),
#   ntimes = 200,
#   control = list(
#     maxiter=50000, tol=1e-10,
#     conv.spec=function(xold, xnew, lold, lnew, tol){
#       lnew < tol
#     }
#   ),
#   control.spec = list(qn=list(qn=2)),
#   mat="Marshall"
# )
# summary(res_m3, loss_tol=1e-7)

# # order 10
# set.seed(54321)
# res_h10 = benchmark(
#   "sinkhorn",
#   algorithm=c("raw", "daarem", "squarem", "nes", "qn", "pem"),
#   ntimes = 200,
#   control = list(
#     maxiter=50000, tol=1e-10,
#     conv.spec=function(xold, xnew, lold, lnew, tol){
#       lnew < tol
#     }
#   ),
#   control.spec = list(qn=list(qn=5)),
#   mat="Hessenberg",
#   order=10
# )
# summary(res_h10, loss_tol=1e-8)


# # order 20
# set.seed(54321)
# res_h20 = benchmark(
#   "sinkhorn",
#   algorithm=c("raw", "daarem", "squarem", "nes", "qn", "pem"),
#   ntimes = 200,
#   control = list(
#     maxiter=50000, tol=1e-10,
#     conv.spec=function(xold, xnew, lold, lnew, tol){
#       lnew < tol
#     }
#   ),
#   control.spec = list(qn=list(qn=5)),
#   mat="Hessenberg",
#   order=20
# )
# summary(res_h20, loss_tol=1e-8)


# # order 50
# set.seed(54321)
# res_h50 = benchmark(
#   "sinkhorn",
#   algorithm=c("raw", "daarem", "squarem", "nes", "qn", "pem"),
#   ntimes = 200,
#   control = list(
#     maxiter=50000, tol=1e-10,
#     conv.spec=function(xold, xnew, lold, lnew, tol){
#       lnew < tol
#     }
#   ),
#   control.spec = list(qn=list(qn=5)),
#   mat="Hessenberg",
#   order=50
# )
# summary(res_h50, loss_tol=1e-8)
