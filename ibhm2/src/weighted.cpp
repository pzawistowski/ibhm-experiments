#include <Rcpp.h> 
using namespace Rcpp;


// [[Rcpp::export]]
double weightedMean(const NumericVector& x, const NumericVector& w) {
  double val = 0.0;
  double sum = 0.0;
  for(int i=0; i < x.size(); i++){
    if(w[i]>0){
      val += x[i]*w[i];
      sum += w[i];
    }
  }
  if(sum < 1e-10){
    sum = 1.0;
  }
  
  return val/sum;
}

// [[Rcpp::export]]
double weightedVar(const NumericVector& x, const NumericVector& w) { 
  double wm = weightedMean(x,w);
  
  
  double val = 0.0;
  double sum = 0.0;
  double sumSq = 0.0;
  for(int i=0; i < x.size(); i++){
    double tmp = (x[i]-wm);
    val += tmp*tmp*w[i];
    sum += w[i];
    sumSq += w[i]*w[i];
  }
  
  if(sum < 1e-10){
    sum = 1.0;
  }
  
  if(sumSq < 1e-10){
    sumSq = 1.0;
  }
  
  
  return (sum*val)/(sum*sum - sumSq);  
}

// [[_Rcpp::export]]
double weightedVar2(const NumericVector& x, const NumericVector& w, double wm) {     
  
  double val = 0.0;
  double sum = 0.0;
  double sumSq = 0.0;
  for(int i=0; i < x.size(); i++){
    if(w[i]>0){
      double tmp = (x[i]-wm);
      val += tmp*tmp*w[i];
      sum += w[i];
      sumSq += w[i]*w[i];
    }
  }
  
  if(sum < 1e-10){
    sum = 1.0;
  }
  
  if(sumSq < 1e-10){
    sumSq = 1.0;
  }
  
  
  return (sum*val)/(sum*sum - sumSq);  
}

// [[Rcpp::export]]
double weightedCov(const NumericVector& x, const NumericVector& z, const NumericVector& w) { 
  double xm = weightedMean(x,w);
  double zm = weightedMean(z,w);
  
  
  double val = 0.0;
  double sum = 0.0;
  double sumSq = 0.0;
  for(int i=0; i < x.size(); i++){        
    val += (x[i]-xm)*(z[i]-zm)*w[i];
    sum += w[i];
    sumSq += w[i]*w[i];
  }
  
  if(sum < 1e-10){
    sum = 1.0;
  }
  
  if(sumSq < 1e-10){
    sumSq = 1.0;
  }
  
  
  return (sum*val)/(sum*sum - sumSq);  
}

// [[_Rcpp::export]]
double weightedCov2(const NumericVector& x, const NumericVector& z, const NumericVector& w, double xm, double zm) { 
  
  double val = 0.0;
  double sum = 0.0;
  double sumSq = 0.0;
  for(int i=0; i < x.size(); i++){    
    if(w[i]>0){
      val += (x[i]-xm)*(z[i]-zm)*w[i];
      sum += w[i];
      sumSq += w[i]*w[i];
    }
  }
  
  if(sum < 1e-10){
    sum = 1.0;
  }
  
  if(sumSq < 1e-10){
    sumSq = 1.0;
  }
  
  
  return (sum*val)/(sum*sum - sumSq);  
}



// [[Rcpp::export]]
double weightedR(const NumericVector& y1, const NumericVector& y2, const NumericVector& w) {
  
  return weightedCov(y1,y2,w)/sqrt(weightedVar(y1,w)*weightedVar(y2,w));
  
}

double min(double v1, double v2){
  if(v1<v2){
    return v1;
  }else{
    return v2;
  }
}

double sign(double v){
  if(v<0.0){
    return -1;
  }else if(v>0.0){
    return 1;
  }else{
    return 0.0;
  }
}

// [[Rcpp::export]]
double weightedKendall(const NumericVector& y1Ranked, const NumericVector& y2Ranked, const NumericVector& w) {
    
  double sum = 0.0;
  double total = 0.0;
  
  for(int i=0; i < y1Ranked.size(); i++){    
    for(int j=i+1; j < y1Ranked.size(); j++){
      double currW = min(w[i], w[j]);
      if(currW>0){
        sum += currW;
        double diff1 = y1Ranked[i] - y1Ranked[j];
        double diff2 = y2Ranked[i] - y2Ranked[j];
        
        total += currW * sign(diff1*diff2);
      }
    }
  }
  
  if(sum < 1e-10){
    return 0.0;
  }else{
    return total/sum;    
  }
  
  
  
  
}


// [[Rcpp::export]]
NumericVector tiedRanks(const NumericVector& x){
  NumericVector rank(x.length());
  NumericVector greater(x.length());
  NumericVector equals(x.length());  
  
  int n = x.length();
  for(int i=0; i < n; i++){

  	double val = x[i];
		int gr  = greater[i];
		int eq = 1+equals[i];		

		for(int j=i+1; j < n; j++){
			if(x[j] > val){
				gr++;        
			}else if(x[j] == val){
				eq++;
        equals[j]++;
			}else{
        greater[j]++;
			}
		}		
		rank[i] = (double)(n-gr+n-gr-eq+1)/2;
	}
  return rank;
}


// [[Rcpp::export]]
double weightedRho(const NumericVector& y1, const NumericVector& y2, const NumericVector& w) {
  NumericVector yr1 = tiedRanks(y1);
  NumericVector yr2 = tiedRanks(y2);

  return weightedR(yr1,yr2,w);
  
}


class ScalEvaluator{
  public:
    ScalEvaluator(const NumericVector& yr){
      updateResidual(yr);
    }
    
    void updateResidual(const NumericVector& yr){
      yrRanked = tiedRanks(yr);      
    }
    
    void addScal(const NumericVector& z){            
      zRankedList.push_back(tiedRanks(z));
    }
    
    double evaluate(const NumericVector& zCandidateVect, const NumericVector& w){      
      RankedMoments zCand(zCandidateVect, w);
      RankedMoments yr(yrRanked, w, true);
      
      double result = 0.0;
      
      if( yr.var > 1e-5 &&  zCand.var > 1e-5){                                
        double indep = 1.0;
        
        RankedMoments zCand(zCandidateVect, w);
        for(std::list<NumericVector>::iterator it = zRankedList.begin(); 
              it != zRankedList.end(); it++ ){          
      
          RankedMoments z(*it,w, true);
      
          indep *= (1-fabs(weightedCov2(z.ranked, zCand.ranked, w, z.mean, zCand.mean)/sqrt(zCand.var*z.var)));      
        }
        if(!R_FINITE(indep)){
          indep = 1.0;  
        }       
                
        double cor = weightedCov2(yr.ranked, zCand.ranked,w, yr.mean, zCand.mean)/sqrt(zCand.var*yr.var);
       
        if(!R_FINITE(cor)){
          cor  = 0;
        }
        
        result = (fabs(cor) * indep);        
      }
      
      return result;
    }
  private:
    void tiedRanks2(NumericVector& x){      
      NumericVector& rank = x;
      NumericVector greater(x.length());
      NumericVector equals (x.length());  
      
      int n = x.length();
      for(int i=0; i < n; i++){
    
        double val = x[i];
    		int gr  = greater[i];
    		int eq = 1+equals[i];		
    
    		for(int j=i+1; j < n; j++){
    			if(x[j] > val){
    				gr++;        
    			}else if(x[j] == val){
    				eq++;
            equals[j]++;
    			}else{
            greater[j]++;
    			}
    		}		
    		rank[i] = (double)(n-gr+n-gr-eq+1)/2;
    	}      
    }
       
    
    class RankedMoments{
        public:
          double var;
          double mean;
          NumericVector ranked;          
          
          RankedMoments(const NumericVector& v, const NumericVector& w, bool is_ranked = false) {            
            
            if(is_ranked){
              ranked = v;
            }else{
              ranked = tiedRanks(v);
            }
            
            mean = weightedMean(ranked,w);
            var = weightedVar2(ranked,w,mean);
            
          }
          
      };
    
  
  private:
    NumericVector yrRanked;
    std::list<NumericVector> zRankedList;
};
