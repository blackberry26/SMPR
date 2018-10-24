- [Метрические алгоритмы классификации](https://github.com/blackberry26/SMPR#Метрические-алгоритмы-классификации)
  - [Метод ближайшего соседа (1NN)](https://github.com/blackberry26/SMPR#Метод-ближайшего-соседа-1nn)
  - [Метод k ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-ближайших-соседейknn)
  - [Метод k взвешенных ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-взвешенных-ближайших-соседей-kwnn)
# Метрические алгоритмы классификации

**Гипотеза компактности.** Схожим объектам соответствуют схожие ответы.

Для формализации понятия сходства вводится функция расстояния в пространстве объектов **Х**.


Имеется пространство объектов ![1111](https://user-images.githubusercontent.com/43415122/46093675-a1f6fc80-c1c0-11e8-8492-d1212656c198.png) и конечное множество имён классов ![11111](https://user-images.githubusercontent.com/43415122/46093711-b935ea00-c1c0-11e8-90bb-9a4716f8164c.png)
.
На множестве ![1111](https://user-images.githubusercontent.com/43415122/46093675-a1f6fc80-c1c0-11e8-8492-d1212656c198.png) задана функция расстояния ![11](https://user-images.githubusercontent.com/43415122/46093571-57758000-c1c0-11e8-92de-21dbb32a0a23.png)
Существует целевая зависимость ![111](https://user-images.githubusercontent.com/43415122/46093631-812ea700-c1c0-11e8-9372-60fb9c754406.png), значения которой известны только на объектах обучающей выборки![1](https://user-images.githubusercontent.com/43415122/46093396-e33adc80-c1bf-11e8-8f8d-179c7576fc9a.png)
 Требуется построить алгоритм классификации ![111111](https://user-images.githubusercontent.com/43415122/46093758-db2f6c80-c1c0-11e8-872b-ce1fc27e56f8.png) , аппроксимирующий целевую зависимость ![1111111](https://user-images.githubusercontent.com/43415122/46093786-f26e5a00-c1c0-11e8-8acb-3f1e34f336c1.png) на всём множестве ![1111](https://user-images.githubusercontent.com/43415122/46093675-a1f6fc80-c1c0-11e8-8492-d1212656c198.png).
 
 Метрические алгоритмы классификации основаны на анализе сходства объектов с помощью функции расстояния, т.е. чем меньше расстояние, тем больше объекты похожи друг на друга.
 
 # Метод ближайшего соседа (1NN)
 *Алгоритм ближайшего соседа* - самый простой алгоритм классификации. Он относит классифицируемый объект ![11111111](https://user-images.githubusercontent.com/43415122/46094409-8856b480-c1c2-11e8-9dd0-3ceefbc706df.png) к тому
классу, которому принадлежит ближайший обучающий объект:
![111111111](https://user-images.githubusercontent.com/43415122/46094442-a8867380-c1c2-11e8-8c37-5641da90f94a.png)

**Достоинства:**
- простота реализации

**Недостатки:**
- неустойчивость к погрешностям
- отсутствие параметров, которые можно было бы настраивать по выборке.


![1NN](https://user-images.githubusercontent.com/43415122/46460406-2565ae80-c7bc-11e8-8388-b0603bbddc08.png)



[Code](https://github.com/blackberry26/SMPR/blob/master/on%D1%82%D1%82e.R)

 # Метод k ближайших соседей (KNN)
В предыдущем методе учитывался только один сосед,теперь рассмотрим алгоритм, который использует *k* ближайших соседей.  
**KNN** - метрический алгоритм классификации, где в качестве оценки близости объекта ![1](https://user-images.githubusercontent.com/43415122/46746639-231dbb80-ccb8-11e8-8c33-c0a871926339.png) к классу ![2](https://user-images.githubusercontent.com/43415122/46746682-36c92200-ccb8-11e8-965b-d35e244a31e6.png) выступает функция-индикатор ![3](https://user-images.githubusercontent.com/43415122/46746693-4183b700-ccb8-11e8-8cba-76d363c8cb66.png) где ![4](https://user-images.githubusercontent.com/43415122/46746713-4f393c80-ccb8-11e8-988c-bcaa48ceaefc.png) — порядок ближайшего соседа к классифицируемой точке ![1](https://user-images.githubusercontent.com/43415122/46746639-231dbb80-ccb8-11e8-8c33-c0a871926339.png).

Проще говоря, алгоритм выбирает **k** ближайших соседей и возвращает тот класс, который среди выбранных встречается большее количество раз.
Для классификации используется стандартная выборка ирисов Фишера по лепесткам.

**Достоинства:**
- прост в реализации
- неплохие результаты при правильно подобраном k

**Недостатки:**
- необходимо хранить всю выборку целиком
- бедный набор параметров
- не все точки с одинаковым расстоянием будут учитаны
- примитивная оценка близости
- в случае одинаковых весов классов алгоритм выбирает любой

[Code](https://github.com/blackberry26/SMPR/blob/master/knn.R)
![photofacefun_com_1539847206](https://user-images.githubusercontent.com/43415122/47137513-12bca080-d2b7-11e8-9b6d-ca33208f367b.jpg)

# Метод k взвешенных ближайших соседей (KWNN)
Метод **K взвешенных ближайших соседей** - это метрический алгоритм классификации, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки. 

 В качестве оценки близости объекта U к классу Y выступает функция ![yeyer](https://user-images.githubusercontent.com/43415122/47447139-897d0080-d7c5-11e8-96a8-fd9a270deeef.png) где
  - **i** порядок ближайшего соседа к классифицируемой точке **u**; 
  - **w(i)** — вес относительно точки **u**, строго убывающая функция.
  
 *Алгоритм kwKNN* зависит от параметра k, который варьируется от k = 1  до k = l, и выбора весовой фунции. Вlданном примере была выбрана следующая функция:
 ![yeyer](https://user-images.githubusercontent.com/43415122/47447385-08723900-d7c6-11e8-8d74-7b7fa155eaec.png)
 
 **Достоинства**

- простота реализации
- классификацию, проведенную данным алгоритмом, легко интерпретировать путём предъявления пользователю нескольких ближайших объектов

**Недостатки**

- неэффективный расход памяти 
- необходимость хранения обучающей выборки целиком
- поиск ближайшего соседа предполагает сравнение классифицируемого объекта со всеми объектами выборки, что требует линейного по длине выборки числа операций
