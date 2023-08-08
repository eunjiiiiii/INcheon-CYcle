# INcheon-CYcle
제6회 인천광역시 공공데이터 활용 창업 분석 경진대회 우수상🥈
<br>
2020.07 ~ 2020.08
<br>
<br>

---
## 1. 요약
인천시 공공 자전거 서비스 '인싸(인천 자전거(Incheon Cycle)의 줄임말)'는 인천 8개의 구 전체에 확대하여 공공 자전거를 운영하는 것으로, 빅데이터를 기반으로 수요를 미리 예측하여 공공 자전거 대여소를 설치하는 것이 핵심입니다.

---
## 2. 핵심 내용
공공 자전거 대여소 위치는 많은 사람들이 이용할 수 있는 곳에 가장 큰 효과를 볼 수 있다고 생각하였습니다.
따라서 우리는 **인천시에 가상 대여소를 설치한 후, 빅데이터를 기반으로 이용자수를 미리 예측하여 수요가 클 것이라 예측되는 곳을 최종 초기 대여소로 선정하는 것을 목표**로 하였습니다. 
인천시에는 공공 자전거 대여소가 없기에 초기 대여소를 설치함에 신중해야 합니다. 
초기 대여소를 설치함에 앞서 시민들의 이용이 편리하도록 주변 생활 시설에 접근성이 높은 '**지하철역 출입구, 고등학교(공공자전거 이용 가능 나이가 만 15세 이상인 것을 고려), 공원**' 근처에 총 **235개의 가상 초기 대여소**를 생성하였습니다.


![가상대여소](https://github.com/eunjiiiiii/INcheon-CYcle/assets/47842737/833de454-9d7e-415d-b827-2cee730c1ac8)



하지만 주변 생활 시설의 접근성 뿐만 아니라 대여소 근처의 상권, 인구, 시설을 고려하여 많은 인천 시민 및 관광객들이 '인싸'를 활용하기 편리하게 대여소를 설치해야 합니다. 
따라서 상권, 인구, 시설을 고려하기 위해 임의로 세운 가상 대여소 반경 100m 이내의 데이터를 수집하였습니다.


수집한 데이터는 다음과 같습니다.


- **상권 정보**
  : 전체 업소 수, 음식점 업소  수, 서비스 업소 수, 도/소매점 업소 수
- **인구 정보**
  : 유동 인구 수, 주거 인구 수, 직장 인구 수, 유동 인구 중 연령대별 비율(10대, 20대, 30대, 40대, 50대, 60대), 유동 인구의 높은 수를 차지하는 성별 데이터, 주거 인구의 소득 및 소비 평균과 직장 인구의 소득 및 소비 평균
- **시설 정보**
  : 공공 기관 수, 금융 기관 수, 의료/복지 기관 수, 학교 수, 교통시설(지하철역+버스정류장) 수, 공원수, 공원의 면적


저희는 이 데이터를 기반으로 **가상 대여소의 수요를 예측하는 모델**을 생성했습니다.


가상 대여소의 수요인 이용자 수를 예측하기 위해서는 설득력 있는 모델을 통해 이용자 수를 예측하는 것이 바람직합니다.
우리가 제안하는 인천시 공공 자전거 서비스 '인싸'의 경우, 공공 자전거 대여소가 없기에 **초기 대여소**를 세우는 것이 우선입니다.
초기 대여소의 경우 실제 이용자 수를 알 수 없어 모델을 생성해도 예측값과 실제값을 비교하는 것이 불가능하여 모델의 정확도를 측정할 수 없습니다. 따라서 대여소 이용자 수를 예측해주는 모델을 생성함에 있어 실제 이용자 수 정보를 얻을 수 있는 **서울시의 따릉이 대여소 정보와 창원시의 누비자 대여소 정보**를 이용하였습니다. 


계절 별 이용자 수를 종속변수로 하여 Catboost 회귀 모델을 통해 이용자 수를 예측하는 모델을 생성했고, 모델의 정확도는 0.8668임을 알 수 있어 꽤 정확한 이용자 수를 예측한다고 판단할 수 있었습니다.  

![image](https://github.com/eunjiiiiii/INcheon-CYcle/assets/47842737/662cf353-4252-4c76-9d69-95e8309c6150)
![image](https://github.com/eunjiiiiii/INcheon-CYcle/assets/47842737/b3a42b32-1176-4b2e-95dd-5b301cd84e07)


아래는 생성한 이용자 수 예측 모델에 인천시 가상 대여소 데이터를 넣어 예측한 이용자 수를 통해 이용자 수가 높은 **150개의 대여소**를 선정하여 지도에 나타낸 그림입니다. 이 대여소들을 실제 초기 대여소로 설정하는 것이 '인싸' 초기 대여소 설립 과정입니다.


![예측이용자수기반_가상대여소](https://github.com/eunjiiiiii/INcheon-CYcle/assets/47842737/91ba5946-894a-469e-8863-d4c164981fd4)

