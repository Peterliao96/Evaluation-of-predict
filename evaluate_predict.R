library(forecast)
set.seed(443)
# initialize counters for the size 500 and 20, the counter is set to
# size 4 is becuase we want to predict the value with steps h = 1,2,5,10
counter11_500_sigma1<-numeric(4)
counter13_500_sigma1<-numeric(4)
counter31_500_sigma1<-numeric(4)
counter33_500_sigma1<-numeric(4)
counter11_500_sigmasqrt3<-numeric(4)
counter13_500_sigmasqrt3<-numeric(4)
counter33_500_sigmasqrt3<-numeric(4)
counter31_500_sigmasqrt3<-numeric(4)
counter11_20_sigma1<-numeric(4)
counter13_20_sigma1<-numeric(4)
counter33_20_sigma1<-numeric(4)
counter31_20_sigma1<-numeric(4)
counter11_20_sigmasqrt3<-numeric(4)
counter13_20_sigmasqrt3<-numeric(4)
counter33_20_sigmasqrt3<-numeric(4)
counter31_20_sigmasqrt3<-numeric(4)

avg_mse_500_ARMA11_sigma1<-numeric(4)
avg_mse_500_ARMA13_sigma1<-numeric(4)
avg_mse_500_ARMA33_sigma1<-numeric(4)
avg_mse_500_ARMA31_sigma1<-numeric(4)
avg_mse_500_ARMA11_sigmasqrt3<-numeric(4)
avg_mse_500_ARMA13_sigmasqrt3<-numeric(4)
avg_mse_500_ARMA33_sigmasqrt3<-numeric(4)
avg_mse_500_ARMA31_sigmasqrt3<-numeric(4)
avg_mse_20_ARMA11_sigma1<-numeric(4)
avg_mse_20_ARMA13_sigma1<-numeric(4)
avg_mse_20_ARMA33_sigma1<-numeric(4)
avg_mse_20_ARMA31_sigma1<-numeric(4)
avg_mse_20_ARMA11_sigmasqrt3<-numeric(4)
avg_mse_20_ARMA13_sigmasqrt3<-numeric(4)
avg_mse_20_ARMA33_sigmasqrt3<-numeric(4)
avg_mse_20_ARMA31_sigmasqrt3<-numeric(4)

# number for ARMA(1,1),ARMA(1,3), ARMA(3,3), ARMA(3,1)
ARMA11_sigma1<-c(5,13)
ARMA13_sigma1<-c(6,14)
ARMA33_sigma1<-c(7,15)
ARMA31_sigma1<-c(8,16)
ARMA11_sigmasqrt3<-c(1,9)
ARMA13_sigmasqrt3<-c(2,10)
ARMA33_sigmasqrt3<-c(3,11)
ARMA31_sigmasqrt3<-c(4,12)


# timesteps ahead
h<-c(1,2,5,10)

# The outerloop is looping through the timesteps vector h
for (k in 1:4) {
  # intialize  mse for the size 500 and 20 with sigma=1,sqrt(3)
  # respectively. The vector size is set to 500 because we want
  # to simulate 500 times
  mse_500_ARMA11_sigma1<-numeric(500)
  mse_500_ARMA13_sigma1<-numeric(500)
  mse_500_ARMA33_sigma1<-numeric(500)
  mse_500_ARMA31_sigma1<-numeric(500)
  mse_500_ARMA11_sigmasqrt3<-numeric(500)
  mse_500_ARMA13_sigmasqrt3<-numeric(500)
  mse_500_ARMA33_sigmasqrt3<-numeric(500)
  mse_500_ARMA31_sigmasqrt3<-numeric(500)
  mse_20_ARMA11_sigma1<-numeric(500)
  mse_20_ARMA13_sigma1<-numeric(500)
  mse_20_ARMA33_sigma1<-numeric(500)
  mse_20_ARMA31_sigma1<-numeric(500)
  mse_20_ARMA11_sigmasqrt3<-numeric(500)
  mse_20_ARMA13_sigmasqrt3<-numeric(500)
  mse_20_ARMA33_sigmasqrt3<-numeric(500)
  mse_20_ARMA31_sigmasqrt3<-numeric(500)
  
  # The innerloop simulates 500 times, which is looping through
  # the generated time series data
  for (j in 1:500) {
  # generate training data from the model
    t1<-arima.sim(model = list(order=c(1,0,1), ar=0.3,ma=0.3, sigma = sqrt(3)), n=500)
    t2<-arima.sim(model = list(order=c(1,0,3), ar=0.3,ma=c(0.1,-0.2,0.3),sigma = sqrt(3)), n=500)
    t3<-arima.sim(model = list(order=c(3,0,3), ar=c(0.3,-0.2,0.2),ma=c(0.1,-0.2,0.3),sigma=sqrt(3)),n=500)
    t4<-arima.sim(model = list(order=c(3,0,1), ar=c(0.3,-0.2,0.2),ma=0.3, sigma = sqrt(3)),n=500)
    t5<-arima.sim(model = list(order=c(1,0,1), ar=0.3,ma=0.3, sigma = 1), n=500)
    t6<-arima.sim(model = list(order=c(1,0,3), ar=0.3,ma=c(0.1,-0.2,0.3),sigma = 1), n=500)
    t7<-arima.sim(model = list(order=c(3,0,3), ar=c(0.3,-0.2,0.2),ma=c(0.1,-0.2,0.3),sigma=1),n=500)
    t8<-arima.sim(model = list(order=c(3,0,1), ar=c(0.3,-0.2,0.2),ma=0.3, sigma = 1),n=500)
    t9<-arima.sim(model = list(order=c(1,0,1), ar=0.3,ma=0.3, sigma = sqrt(3)), n=20)
    t10<-arima.sim(model = list(order=c(1,0,3), ar=0.3,ma=c(0.1,-0.2,0.3), sigma = sqrt(3)), n=20)
    t11<-arima.sim(model = list(order=c(3,0,3), ar=c(0.3,-0.2,0.2),ma=c(0.1,-0.2,0.3),sigma = sqrt(3)),n=20)
    t12<-arima.sim(model = list(order=c(3,0,1), ar=c(0.3,-0.2,0.2),ma=0.3, sigma = sqrt(3)),n=20)
    t13<-arima.sim(model = list(order=c(1,0,1), ar=0.3,ma=0.3, sigma = 1), n=20)
    t14<-arima.sim(model = list(order=c(1,0,3), ar=0.3,ma=c(0.1,-0.2,0.3), sigma = 1), n=20)
    t15<-arima.sim(model = list(order=c(3,0,3), ar=c(0.3,-0.2,0.2),ma=c(0.1,-0.2,0.3),sigma = 1),n=20)
    t16<-arima.sim(model = list(order=c(3,0,1), ar=c(0.3,-0.2,0.2),ma=0.3, sigma = 1),n=20)

    # model selection for the time series with size 500 and 
    # 20 respectively
    
    # combine time series dataset of the same size so as 
    # to partition the time series data easily in the 
    # for loop below
    total_tseries500_sigma1<-c(t5,t6,t7,t8)
    total_tseries500_sigmasqrt3<-c(t1,t2,t3,t4)
    total_tseries20_sigma1<-c(t13,t14,t15,t16)
    total_tseries20_sigmasqrt3<-c(t9,t10,t11,t12)
    
    # stores AIC value in two vectors
    v_AIC500_sigma1<-numeric(4)
    v_AIC500_sigmasqrt3<-numeric(4)
    v_AIC20_sigma1<-numeric(4)
    v_AIC20_sigmasqrt3<-numeric(4)
    
    for (i in 1:4){
      # ith time series dataset with size 500
      ti500_sigma1<-total_tseries500_sigma1[(500*i-499):(500*i)]
      ti500_sigmasqrt3<-total_tseries500_sigmasqrt3[(500*i-499):(500*i)]
      
      # fit the ith time series model
      fit_ti500_sigma1<-arima(ti500_sigma1)
      AIC_ti500_sigma1<-AIC(fit_ti500_sigma1)
      v_AIC500_sigma1[i]<-AIC_ti500_sigma1
      fit_ti500_sigmasqrt3<-arima(ti500_sigmasqrt3)
      AIC_ti500_sigmasqrt3<-AIC(fit_ti500_sigmasqrt3)
      v_AIC500_sigmasqrt3[i]<-AIC_ti500_sigmasqrt3
      
      # ith time series dataset with size 20
      ti20_sigma1<-total_tseries20_sigma1[(20*i-19):(20*i)]
      ti20_sigmasqrt3<-total_tseries20_sigmasqrt3[(20*i-19):(20*i)]
      
      # fit the ith time series model
      fit_ti20_sigma1<-arima(ti20_sigma1)
      AIC_ti20_sigma1<-AIC(fit_ti20_sigma1)
      v_AIC20_sigma1[i]<-AIC_ti20_sigma1
      fit_ti20_sigmasqrt3<-arima(ti20_sigmasqrt3)
      AIC_ti20_sigmasqrt3<-AIC(fit_ti20_sigmasqrt3)
      v_AIC20_sigmasqrt3[i]<-AIC_ti20_sigmasqrt3
    }
    
    # nth model to be selected for the sample size 500, with 
    # the minimum of AIC
    n_sigma1<-which.min(v_AIC500_sigma1)
    n_sigmasqrt3<-which.min(v_AIC500_sigmasqrt3)
    
    # predicts h[k] ahead and generates mse_500
    tn_sigma1<-total_tseries500_sigma1[(500*n_sigma1-499):(500*n_sigma1)]
    fit1_sigma1<-arima(tn_sigma1)
    pred_fit1_sigma1<-predict(fit1_sigma1,n.ahead=h[k])
    pred_fit1_sigma1$pred
    fit1_sigma1_pred_val<-pred_fit1_sigma1$pred[1]
    mse1_sigma1<-mean((tn_sigma1-fit1_sigma1_pred_val)^2)
    
    tn_sigmasqrt3<-total_tseries500_sigmasqrt3[(500*n_sigmasqrt3-499):(500*n_sigmasqrt3)]
    fit1_sigmasqrt3<-arima(tn_sigmasqrt3)
    pred_fit1_sigmasqrt3<-predict(fit1_sigmasqrt3,n.ahead=h[k])
    pred_fit1_sigmasqrt3$pred
    fit1_sigmasqrt3_pred_val<-pred_fit1_sigmasqrt3$pred[1]
    mse1_sigmasqrt3<-mean((tn_sigmasqrt3-fit1_sigmasqrt3_pred_val)^2)
    
    # counter counts how many times the model is selected
    # in order to compute the proportion
    if (is.element((n_sigma1+4),ARMA11_sigma1)) {
      counter11_500_sigma1[k]<-counter11_500_sigma1[k] + 1
      mse_500_ARMA11_sigma1[counter11_500_sigma1[k]]<-sqrt(mse1_sigma1)
    } else if (is.element((n_sigma1+4), ARMA13_sigma1)) {
      counter13_500_sigma1[k]<-counter13_500_sigma1[k] + 1
      mse_500_ARMA13_sigma1[counter13_500_sigma1[k]]<-sqrt(mse1_sigma1)
    } else if (is.element((n_sigma1+4), ARMA33_sigma1)) {
      counter33_500_sigma1[k]<-counter33_500_sigma1[k] + 1
      mse_500_ARMA33_sigma1[counter33_500_sigma1[k]]<-sqrt(mse1_sigma1)
    } else if (is.element((n_sigma1+4), ARMA31_sigma1)) {
      counter31_500_sigma1[k]<-counter31_500_sigma1[k] + 1
      mse_500_ARMA31_sigma1[counter31_500_sigma1[k]]<-sqrt(mse1_sigma1)
    } 
    if (is.element(n_sigmasqrt3, ARMA11_sigmasqrt3)) {
      counter11_500_sigmasqrt3[k]<-counter11_500_sigmasqrt3[k]+1
      mse_500_ARMA11_sigmasqrt3[counter11_500_sigmasqrt3[k]]<-sqrt(mse1_sigmasqrt3)
    } else if (is.element(n_sigmasqrt3, ARMA13_sigmasqrt3)) {
      counter13_500_sigmasqrt3[k]<-counter13_500_sigmasqrt3[k]+1
      mse_500_ARMA13_sigmasqrt3[counter13_500_sigmasqrt3[k]]<-sqrt(mse1_sigmasqrt3)
    } else if (is.element(n_sigmasqrt3, ARMA33_sigmasqrt3)) {
      counter33_500_sigmasqrt3[k]<-counter33_500_sigmasqrt3[k]+1
      mse_500_ARMA33_sigmasqrt3[counter33_500_sigmasqrt3[k]]<-sqrt(mse1_sigmasqrt3)
    } else if (is.element(n_sigmasqrt3, ARMA31_sigmasqrt3)) {
      counter31_500_sigmasqrt3[k]<-counter31_500_sigmasqrt3[k]+1
      mse_500_ARMA31_sigmasqrt3[counter31_500_sigmasqrt3[k]]<-sqrt(mse1_sigmasqrt3)
    }
    
    
    # mth model to be selected for the sample size 20, with
    # the minimum of AIC
    m_sigma1<-which.min(v_AIC20_sigma1)
    m_sigmasqrt3<-which.min(v_AIC20_sigmasqrt3)
    
    # predicts h[k] ahead and generates mse_20
    tm_sigma1<-total_tseries20_sigma1[(20*m_sigma1-19):(20*m_sigma1)]
    fit2_sigma1<-arima(tm_sigma1)
    pred_fit2_sigma1<-predict(fit2_sigma1,n.ahead=h[k])
    pred_fit2_sigma1$pred
    fit2_sigma1_pred_val<-pred_fit2_sigma1$pred[1]
    mse2_sigma1<-mean((tm_sigma1-fit2_sigma1_pred_val)^2)
    
    tm_sigmasqrt3<-total_tseries20_sigmasqrt3[(20*m_sigmasqrt3-19):(20*m_sigmasqrt3)]
    fit2_sigmasqrt3<-arima(tm_sigmasqrt3)
    pred_fit2_sigmasqrt3<-predict(fit2_sigmasqrt3,n.ahead=h[k])
    pred_fit2_sigmasqrt3$pred
    fit2_sigmasqrt3_pred_val<-pred_fit2_sigmasqrt3$pred[1]
    mse2_sigmasqrt3<-mean((tm_sigmasqrt3-fit2_sigmasqrt3_pred_val)^2)
    
    # counter counts how many times the model is selected
    # in order to compute the proportion
    if (is.element((m_sigma1+12),ARMA11_sigma1)) {
      counter11_20_sigma1[k]<-counter11_20_sigma1[k] + 1
      mse_20_ARMA11_sigma1[counter11_20_sigma1[k]]<-sqrt(mse2_sigma1)
    } else if (is.element((m_sigma1+12), ARMA13_sigma1)) {
      counter13_20_sigma1[k]<-counter13_20_sigma1[k] + 1
      mse_20_ARMA13_sigma1[counter13_20_sigma1[k]]<-sqrt(mse2_sigma1)
    } else if (is.element((m_sigma1+12), ARMA33_sigma1)) {
      counter33_20_sigma1[k]<-counter33_20_sigma1[k] + 1
      mse_20_ARMA33_sigma1[counter33_20_sigma1[k]]<-sqrt(mse2_sigma1)
    } else if (is.element((m_sigma1+12), ARMA31_sigma1)) {
      counter31_20_sigma1[k]<-counter31_20_sigma1[k] + 1
      mse_20_ARMA31_sigma1[counter31_20_sigma1[k]]<-sqrt(mse2_sigma1)
    } 
    if (is.element((m_sigmasqrt3+8),ARMA11_sigmasqrt3)) {
      counter11_20_sigmasqrt3[k]<-counter11_20_sigmasqrt3[k]+1
      mse_20_ARMA11_sigmasqrt3[counter11_20_sigmasqrt3[k]]<-sqrt(mse2_sigmasqrt3)
    } else if (is.element((m_sigmasqrt3+8), ARMA13_sigmasqrt3)) {
      counter13_20_sigmasqrt3[k]<-counter13_20_sigmasqrt3[k]+1
      mse_20_ARMA13_sigmasqrt3[counter13_20_sigmasqrt3[k]]<-sqrt(mse2_sigmasqrt3)
    } else if (is.element((m_sigmasqrt3+8), ARMA33_sigmasqrt3)) {
      counter33_20_sigmasqrt3[k]<-counter33_20_sigmasqrt3[k]+1
      mse_20_ARMA33_sigmasqrt3[counter33_20_sigmasqrt3[k]]<-sqrt(mse2_sigmasqrt3)
    } else if (is.element((m_sigmasqrt3+8), ARMA31_sigmasqrt3)) {
      counter31_20_sigmasqrt3[k]<-counter31_20_sigmasqrt3[k]+1
      mse_20_ARMA31_sigmasqrt3[counter31_20_sigmasqrt3[k]]<-sqrt(mse2_sigmasqrt3)
    }
    
  }
  # MC simulation method: take the mean of the mse vector for each ARMA model
  # and size 500 and 20 respectively
  avg_mse_500_ARMA11_sigma1[k]<-sum(mse_500_ARMA11_sigma1)/counter11_500_sigma1
  avg_mse_500_ARMA13_sigma1[k]<-sum(mse_500_ARMA13_sigma1)/counter13_500_sigma1
  avg_mse_500_ARMA33_sigma1[k]<-sum(mse_500_ARMA33_sigma1)/counter33_500_sigma1
  avg_mse_500_ARMA31_sigma1[k]<-sum(mse_500_ARMA31_sigma1)/counter31_500_sigma1
  avg_mse_500_ARMA11_sigmasqrt3[k]<-sum(mse_500_ARMA11_sigmasqrt3)/counter11_500_sigmasqrt3
  avg_mse_500_ARMA13_sigmasqrt3[k]<-sum(mse_500_ARMA13_sigmasqrt3)/counter13_500_sigmasqrt3
  avg_mse_500_ARMA33_sigmasqrt3[k]<-sum(mse_500_ARMA33_sigmasqrt3)/counter33_500_sigmasqrt3
  avg_mse_500_ARMA31_sigmasqrt3[k]<-sum(mse_500_ARMA31_sigmasqrt3)/counter31_500_sigmasqrt3
  avg_mse_20_ARMA11_sigma1[k]<-sum(mse_20_ARMA11_sigma1)/counter11_20_sigma1
  avg_mse_20_ARMA13_sigma1[k]<-sum(mse_20_ARMA13_sigma1)/counter13_20_sigma1
  avg_mse_20_ARMA33_sigma1[k]<-sum(mse_20_ARMA33_sigma1)/counter33_20_sigma1
  avg_mse_20_ARMA31_sigma1[k]<-sum(mse_20_ARMA31_sigma1)/counter31_20_sigma1
  avg_mse_20_ARMA11_sigmasqrt3[k]<-sum(mse_20_ARMA11_sigmasqrt3)/counter11_20_sigmasqrt3
  avg_mse_20_ARMA13_sigmasqrt3[k]<-sum(mse_20_ARMA13_sigmasqrt3)/counter13_20_sigmasqrt3
  avg_mse_20_ARMA33_sigmasqrt3[k]<-sum(mse_20_ARMA33_sigmasqrt3)/counter33_20_sigmasqrt3
  avg_mse_20_ARMA31_sigmasqrt3[k]<-sum(mse_20_ARMA31_sigmasqrt3)/counter31_20_sigmasqrt3
}


# fix sigma and sample size, create 4 by 4 matrices with
# different ARMA(p,q) model and timesteps h
m_500_ARMA_sigma1<-rbind(t(avg_mse_500_ARMA11_sigma1),
                         t(avg_mse_500_ARMA13_sigma1),
                         t(avg_mse_500_ARMA33_sigma1),
                         t(avg_mse_500_ARMA31_sigma1))

colnames(m_500_ARMA_sigma1)<-c('h=1','h=2','h=5','h=10')
rownames(m_500_ARMA_sigma1)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')

m_500_ARMA_sigmasqrt3<-rbind(t(avg_mse_500_ARMA11_sigmasqrt3),
                             t(avg_mse_500_ARMA13_sigmasqrt3),
                             t(avg_mse_500_ARMA33_sigmasqrt3),
                             t(avg_mse_500_ARMA31_sigmasqrt3))

colnames(m_500_ARMA_sigmasqrt3)<-c('h=1','h=2','h=5','h=10')
rownames(m_500_ARMA_sigmasqrt3)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')

m_20_ARMA_sigma1<-rbind(t(avg_mse_20_ARMA11_sigma1),
                        t(avg_mse_20_ARMA13_sigma1),
                        t(avg_mse_20_ARMA33_sigma1),
                        t(avg_mse_20_ARMA31_sigma1))

colnames(m_20_ARMA_sigma1)<-c('h=1','h=2','h=5','h=10')
rownames(m_20_ARMA_sigma1)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')

m_20_ARMA_sigmasqrt3<-rbind(t(avg_mse_20_ARMA11_sigmasqrt3),
                            t(avg_mse_20_ARMA13_sigmasqrt3),
                            t(avg_mse_20_ARMA33_sigmasqrt3),
                            t(avg_mse_20_ARMA31_sigmasqrt3))

colnames(m_20_ARMA_sigmasqrt3)<-c('h=1','h=2','h=5','h=10')
rownames(m_20_ARMA_sigmasqrt3)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')

# create 4 by 4 matrices which represent the proportion 
# for times of model selected

m_counter_500_sigma1<-rbind(t(counter11_500_sigma1),
                            t(counter13_500_sigma1),
                            t(counter33_500_sigma1),
                            t(counter31_500_sigma1))

proportion_500_sigma1<-m_counter_500_sigma1/500
colnames(proportion_500_sigma1)<-c('h=1','h=2','h=5','h=10')
rownames(proportion_500_sigma1)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')

m_counter_500_sigmasqrt3<-rbind(t(counter11_500_sigmasqrt3),
                                t(counter13_500_sigmasqrt3),
                                t(counter33_500_sigmasqrt3),
                                t(counter31_500_sigmasqrt3))

proportion_500_sigmasqrt3<-m_counter_500_sigmasqrt3/500
colnames(proportion_500_sigmasqrt3)<-c('h=1','h=2','h=5','h=10')
rownames(proportion_500_sigmasqrt3)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')

m_counter_20_sigma1<-rbind(t(counter11_20_sigma1),
                           t(counter13_20_sigma1),
                           t(counter33_20_sigma1),
                           t(counter31_20_sigma1))

proportion_20_sigma1<-m_counter_20_sigma1/500
colnames(proportion_20_sigma1)<-c('h=1','h=2','h=5','h=10')
rownames(proportion_20_sigma1)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')

m_counter_20_sigmasqrt3<-rbind(t(counter11_20_sigmasqrt3),
                               t(counter13_20_sigmasqrt3),
                               t(counter33_20_sigmasqrt3),
                               t(counter31_20_sigmasqrt3))

proportion_20_sigmasqrt3<-m_counter_20_sigmasqrt3/500
colnames(proportion_20_sigmasqrt3)<-c('h=1','h=2','h=5','h=10')
rownames(proportion_20_sigmasqrt3)<-c('ARMA(1,1)','ARMA(1,3)','ARMA(3,3)','ARMA(3,1)')


