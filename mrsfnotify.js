/*simple stock to flow notification bot
@phraudsta (c) 2019 MIT Licence
      example run:
        node STOCK2FLOW.js
     THIS IS FOR EDUCATIONAL PURPOSES. THIS IS NOT FINANCIAL ADVICE.
*/
'use strict';
const nodemailer = require('nodemailer');
const jStat = require('jStat').jStat;
const fetch = require('node-fetch');


function mean(values) {
  return (values.reduce((previous, current) => current += previous)/values.length);
}

//returns the (non-parametric) likelihood that there are x nonconforming units in n
    function getNPLikelihood(x,n, alpha) {
      return 1-jStat.beta.inv( alpha, x, n-x+1 );
    }

//simple OLS (Ordinary Least Squares) function
//returns the OLS estimate for the single predictor case y~x
//might be quicker to implement as linear algebra
//but would first need to implement linear algebra functions
function OLS(y,x){
  if(y.length!=x.length) throw  new Error("Y and X are of different lengths");
          var lm = {};
          var n = y.length;
          var xbar = mean(x);
          var ybar = mean(y);
          var beta_num = 0;
          var beta_den = 0;
          var beta1hat = 0;
          var beta0hat = 0;
          var yhat =x;
          var residuals = x;
          var SST = 0;
          var SSE = 0;
          var SSR = 0;
          var R2 = 0;
          var AIC = 0;
          //multiple loops required
          //:-1st to get coefficients
          //:-2nd to get residuals and fitted
          for (var i = 0; i < y.length; i++) {
            if(!Number.isNaN(x[i]) & !Number.isNaN(y[i])) { //skip nans
              beta_num += (x[i]-xbar)*(y[i]-ybar);
              beta_den += Math.pow((x[i]-xbar),2);
            }
          }
        var  beta1hat = beta_num/beta_den;
        var  beta0hat = ybar - beta1hat * xbar;

          for(var i =0;i<y.length;i++) {
            yhat[i]=beta0hat+beta1hat*x[i];
            residuals[i] = y[i]-yhat[i];
            SSE+=Math.pow(residuals[i],2);
            SST+=Math.pow(yhat[i]-y[i],2);
          }

        var  sebeta1hat = Math.sqrt((SSE/(y.length-2))/beta_den);
        var  b1tstat = beta1hat/sebeta1hat

        var  sebeta0hat = Math.sqrt((SSE/(y.length-2))*(Math.pow(xbar,2)/beta_den))
        var  b0tstat = beta0hat/sebeta0hat

          AIC = n*Math.log(SSE/n)+4 //assuming k = 1 for simple case
          SSR = SST-SSE;
          R2 = 1- SSE/SST;

          lm['beta1hat'] = beta1hat;
          lm['beta0hat'] = beta0hat;
          lm['R2'] = R2;
          lm['SSE'] = SSE;
          lm['SST']=SST;
          lm['SSR']=SSR;
          lm['AIC']=AIC;
          lm['Yhat']=yhat;
          lm['residuals']=residuals;
          lm['beta1_tstat']=b1tstat;
          lm['beta0_tstat']=b0tstat;
          return lm;
}



function main() {
//coinmetrics has excellent daily data available.
  let url  = 'https://community-api.coinmetrics.io/v2/assets/btc/metricdata?metrics=SplyCur,PriceUSD';
  var stock=[];
  var price = [];
  var time=[];
  var flow=[];
  var sf = [];
  fetch(url)
  .then(res => res.json())
  .then((out) => {
    var prevStock = 0;
    var n = 0;
    for(var i in out.metricData.series) {
      time.push( out.metricData.series[i].time); //icky time values. will need to reprocess them at some point for pretty graphs
      stock.push(out.metricData.series[i].values[0]*1);
      price.push(out.metricData.series[i].values[1]*1);
      if(out.metricData.series[i].values[1] ==null) n++;
        var f = (out.metricData.series[i].values[0]-prevStock)*365.25; //daily flow
        flow.push(f) ;
        sf.push(out.metricData.series[i].values[0]/f);
      prevStock = out.metricData.series[i].values[0];
    }

    //get OLS:
    //first slice arrays to where we had prices
    time = time.slice(n,time.length);
    price = price.slice(n,price.length);
    sf = sf.slice(n,sf.length);
    var lnprice=price.map(function(e) {e = Math.log(e); return e;});
    var lnsf=sf.map(function(e) {e = Math.log(e); return e;});


    var lm = OLS(lnprice, lnsf);
    console.log("ln(PriceUSD)="+lm.beta0hat+"+"+lm.beta1hat+"*ln(sf)");
    console.log("->PriceUSD="+Math.exp(lm.beta0hat)+"*sf^"+lm.beta1hat);
    console.log("R2:"+lm.R2);
    console.log("AIC:"+lm.AIC);
    console.log("t-stat slope:"+lm.beta1_tstat);
    console.log("t-stat const:"+lm.beta0_tstat);

//ok, now we need to find the likelihood of the current
//residual (eg residual[residual.length]) given
//the previous residuals. The likelihood is found by the
var likelihood={y: [], x:[], name: 'likelihood', type: 'scatter', yaxis: 'y2',  mode: 'lines'};
//this might all seem a bit pointless at the moment, but it will become clear soon enough
for(var i = 0; i< lm.residuals.length; i++){
  var ov = lm.residuals.slice(0,i).filter(counted => counted >= lm.residuals[i]).length; //over valued for high
  var uv = lm.residuals.slice(0,i).filter(counted => counted <= lm.residuals[i]).length; //under valued for low
  if(lm.residuals[i]<0) {
    likelihood.y.push(getNPLikelihood(uv+1,i+1,0.05)); //+1 shift from zero for true df
      } else if(lm.residuals[i]>=0) {
        likelihood.y.push(getNPLikelihood(ov+1,i+1,0.05));
      }
    }
    likelihood.x = time;

    var advice = "";
    if(lm.residuals[lm.residuals.length-1]<0 & likelihood.y[likelihood.y.length-1]>0.8) {
      advice="BUY"
    } else if(lm.residuals[lm.residuals.length-1]>0 & likelihood.y[likelihood.y.length-1]>0.8){
      advice="SELL"
    } else {
      advice="HODL"
    }


    console.log(advice);

         if(advice=="SELL" || advice=="BUY") {

            if(advice=="SELL") {
              //trade actions will go here

              }
            } else {
              //trade actions will go here

            }
    async   function mailer(){


         //let account = await nodemailer.createTestAccount();
      //some places dont need auth, but if u need it put it here
        // create reusable transporter object using the default SMTP transport
        var passw = '_your_email_password_'
        var userw = '_your_email_username_'
        let transporter = nodemailer.createTransport({
          host: "_your_smtp_server_",
          port: 465,
          secure: true, // true for 465, false for other ports
         auth: {
            user: userw, // generated ethereal user
           pass: passw // generated ethereal password
         }
        });

        var outcome = "Residual:"+lm.residuals[lm.residuals.length-1]+"<br />Likelihood:"+likelihood.y[likelihood.y.length-1]
        console.log(outcome);
        let mailOptions = {
          from: '"S2Fbot" <phraudsta2@protonmail.com>', // sender address - you dont have to keep this as my address!
          to: "replace_with@email.address", // list of receivers
          subject: "TA ADVICE: "+advice, // Subject line
          text: outcome, // plain text body
          html: outcome, // html body
        };

          // send mail with defined transport object
          let info = await transporter.sendMail(mailOptions)
          console.log("Message sent: %s", info.messageId);
      }
      mailer().catch(console.error);
  })
  .catch(err => { throw err });



  }


main();
setInterval(function(){main()},86400000); //60 sec x 60 min x 24 hour x 1000 millisec
