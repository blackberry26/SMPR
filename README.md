- [Метрические алгоритмы классификации](https://github.com/blackberry26/SMPR#Метрические-алгоритмы-классификации)
  - [Метод ближайшего соседа (1NN)](https://github.com/blackberry26/SMPR#Метод-ближайшего-соседа-1nn)
  - [Метод k ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-ближайших-соседейknn)
  - [Метод k взвешенных ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-взвешенных-ближайших-соседей-kwnn)
  - [Метод Парзеновского окна](https://github.com/blackberry26/SMPR#Метод-Парзеновского-окна-pw)
  - [Метод потенциальных функций](https://github.com/blackberry26/SMPR#Метод-потенциальных-функций-pf)
  - [Сравнение алгоритмов классификации](https://github.com/blackberry26/SMPR#Сравнение-алгоритмов-классификации)
- [Байесовские классификаторы](https://github.com/blackberry26/SMPR#Байесовские-классификаторы)
  - [Наивный нормальный байесовский классификатор](https://github.com/blackberry26/SMPR#наивный-нормальный-байесовский-классификатор)
  - [Линии уровня нормального распределения](https://github.com/blackberry26/SMPR#линии-уровня-нормального-распределения)
  - [Подстановочный алгоритм](https://github.com/blackberry26/SMPR#подстановочный-алгоритм-plug-in)
  - [Линейный дискриминант Фишера](https://github.com/blackberry26/SMPR#линейный-дискриминант-фишера)
  - [ЕМ-алгоритм](https://github.com/blackberry26/SMPR#ем-алгоритм)
- [Линейные классификаторы](https://github.com/blackberry26/SMPR#линейные-классификаторы)
  - [Метод стохастического градиента](https://github.com/blackberry26/SMPR#метод-стохастического-градиента)
  - [ADALINE](https://github.com/blackberry26/SMPR#adaline)
  - [Логистическая регрессия](https://github.com/blackberry26/SMPR#логистическая-регрессия)
  - [Персептрон Розенблатта](https://github.com/blackberry26/SMPR#персептрон-розенблатта)
  - [Метод опорных векторов](https://github.com/blackberry26/SMPR#метод-опорных-векторов)
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



![47304893-94eaf300-d630-11e8-877f-7b6791c5117d](https://user-images.githubusercontent.com/43415122/47796964-72469180-dd36-11e8-9f30-b840f4d35373.png)

[code](https://github.com/blackberry26/SMPR/blob/master/knn.R)

**LOO(leave-one-out) для Knn**
**LOO**(оценка скользящего контроля) - проверка, которая необходима, чтобы оценить оптимальность алгоритма(выявить при каких *k* оптимален ) и на сколько он ошибается.

Нужно проверить, как часто будет ошибаться алгоритм, если по одному выбирать элементы из обучающей выборки.
Алгоритм состоит в следующем: извлечь элемент, обучить оставшиеся элементы, классифицировать извлеченный, затем вернуть его обратно. Так нужно поступить со всеми элементами выборки.

**График зависимости LOO(при k=6)**

![default](https://user-images.githubusercontent.com/43415122/47837941-72867180-ddb6-11e8-839a-6e8e56a779b5.png) ![photofacefun_com_1541056584](https://user-images.githubusercontent.com/43415122/47838076-e32d8e00-ddb6-11e8-8cdd-cc902cf61209.jpg)

[code](https://github.com/blackberry26/SMPR/blob/master/knnn.R)


# Метод k взвешенных ближайших соседей (KWNN)
Метод **K взвешенных ближайших соседей** - это метрический алгоритм классификации, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки. 

 В качестве оценки близости объекта **u** к классу **Y** выступает функция ![yeyer](https://user-images.githubusercontent.com/43415122/47447139-897d0080-d7c5-11e8-96a8-fd9a270deeef.png) где
  - **i** -порядок ближайшего соседа к классифицируемой точке **u**; 
  - **w(i)** — вес относительно точки **u**, строго убывающая функция.
  
  В качестве весовой функции возьмем нелинейную последовательность, например геометрическую прогрессию:![photofacefun_com_1541660607](https://user-images.githubusercontent.com/43415122/48182794-3d899a00-e335-11e8-9877-f0e9cefcf164.jpg)
  
Возьмем для примера k=6(т.к. в LOO для KNN он наиболее оптимальный),получаем результат при k =6 и q = 1,который равен 0.3.


  
![kwnn](https://user-images.githubusercontent.com/43415122/48185256-1d5dd900-e33d-11e8-9f71-886602c28463.png)



 
 **Достоинства**

- простота реализации
- классификацию, проведенную данным алгоритмом, легко интерпретировать путём предъявления пользователю нескольких ближайших объектов.

**Недостатки**

- неэффективный расход памяти 
- необходимость хранения обучающей выборки целиком
- поиск ближайшего соседа предполагает сравнение классифицируемого объекта со всеми объектами выборки, что требует линейного по длине выборки числа операций

[code](https://github.com/blackberry26/SMPR/blob/master/KWNN.R)

Из-за того, что алгоритм *k взвешенных ближайших соседей* учитывает порядок объектов при классификации, он выдаёт лучший результат, чем алгоритм k ближайших соседей (kNN). Следовательно, объекты **x_i**, которые находятся ближе к классифицируемому объекту **u**, будут оказывать намного большее влияние, чем те **x_i**, которые дальше (из-за учёта порядка объектов).

 # Метод парзеновского окна (PW)
 
 Рассмотрим весовую функцию **w(i,u)** как функцию не от ранга соседа, а от расстояния ![q4](https://user-images.githubusercontent.com/43415122/48507752-db152a00-e85d-11e8-8dd4-bc515803eaa6.png)
 
![q1](https://user-images.githubusercontent.com/43415122/48535492-396cf780-e8b4-11e8-8fc0-0ebebabbd198.png)
где K(z)-функция ядра невозрастающая на![2018-11-15 10-58-39_cut-photo ru](https://user-images.githubusercontent.com/43415122/48535817-33c3e180-e8b5-11e8-88e9-5fb35e2ca7aa.png)

В этом случае метрический классификатор имеет вид: ![q2](https://user-images.githubusercontent.com/43415122/48535883-6c63bb00-e8b5-11e8-9b9c-d17c24c716b3.png).

Алгоритм  ![q3](https://user-images.githubusercontent.com/43415122/48535899-7f768b00-e8b5-11e8-851f-ac48b89f783b.png)
называется алгоритмом парзеновского окна. 

Окно - сферическая окрестность объекта  *u*  радиуса  *h* , при попадании в который обучающий объект  **x_i**  "голосует" за отнесение объекта   *u *  к классу  **y_i**.

*h* -ширина окна,задается априори либо с помощью LOO(h).

Чаще всего применяются 5 типов ядер:

 - Прямоугольное ![68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f522872293d253543667261632537423125374425374232253744253542253743722537432535436c65712673706163653b31253544](https://user-images.githubusercontent.com/43415122/49207845-c9d02f80-f3be-11e8-8b82-d5d57b627f2b.gif)

 ![default](https://user-images.githubusercontent.com/43415122/49180805-cb6e0900-f366-11e8-80af-6e4b1c8d5ea7.png)


 - Квартическое ![rtrt](https://user-images.githubusercontent.com/43415122/49207928-08fe8080-f3bf-11e8-993e-3c4f43ba46dc.gif)


 - Треугольное![ju](https://user-images.githubusercontent.com/43415122/49207873-deacc300-f3be-11e8-9a3b-67513a09f0aa.gif)

 ![default](https://user-images.githubusercontent.com/43415122/49180500-eb50fd00-f365-11e8-88b8-c44ed06dfc0d.png)

 - Епанечникова ![i](https://user-images.githubusercontent.com/43415122/49207889-ee2c0c00-f3be-11e8-9696-b57211d05f28.gif) ![default](https://user-images.githubusercontent.com/43415122/49180695-7500ca80-f366-11e8-971a-6d97799f09ad.png)
 - Гауссовское (нормальное распределение) ![default](https://user-images.githubusercontent.com/43415122/49180753-a4afd280-f366-11e8-9b18-d7154928e105.png)
 
  **Вывод**: Среди всех классифицированных ядер больше всего выделяется гауссовское ядро. Оно однозначно разделило классы на всей плоскости.
  
 [Code](https://github.com/blackberry26/SMPR/blob/master/PW.R)



**Достоинства**
   
 - Простота реализации.
 - Все точки, попадающие в окно, расстояние между которыми одинаково, будут учитываться (в отличие от kWNN).
 - Скорость классификации быстрее, т.к. не требуется сортировка расстояний (O(l)).
 - Окно с переменной шириной решает проблему разрешимости задач, в которых обучающие выборки распределены неравномерно по пространству X.
  

**Недостатки**

 - "Бедный" набор параметров.
 - Если суммарные веса классов оказываются одинаковыми, то алгоритм относит классифицируемый объект u к любому из классов.
 - Слишком узкие окна приводят к неустойчивой классификации, а слишком широкие - к вырождению алгоритма в константу.
 - Диапазон, из которого выбирается параметр h, нужно подбирать самим.
 - Если в окно, радиуса h, не попало ни одной точки x_i, то алгоритм не способен классифицировать объект u.
 
 
 # Метод потенциальных функций (PF)
 
Если в методе парзеновского окна центр окна поместить в обучающие объекты, то получим *метод потенциальных функций.*
Он позволяет с помощью простого алгоритма оценивать вес («важность») объектов обучающей выборки при решении задачи классификации.
Пусть имеется пространство объектов X и конечное множество классов Y. 
На множестве **X** задана функция расстояния ![default](https://user-images.githubusercontent.com/43415122/49168353-57bd0380-f348-11e8-9c6a-51e763adecba.png).

Каждый объект из **X** относится к некоторому классу из **Y** посредством отображения ![default](https://user-images.githubusercontent.com/43415122/49168416-7d4a0d00-f348-11e8-909f-9441410cba17.png).
Пусть также задана обучающая выборка пар «объект—ответ»: ![default](https://user-images.githubusercontent.com/43415122/49168482-a074bc80-f348-11e8-963e-77c4765edb10.png).

Требуется построить алгоритм  ![default](https://user-images.githubusercontent.com/43415122/49168564-ce5a0100-f348-11e8-86f7-f7049cdbdc50.png), который по заданной выборке  ![default](https://user-images.githubusercontent.com/43415122/49168592-e0d43a80-f348-11e8-8c51-286156b22608.png) аппроксимирует отображение ![default](https://user-images.githubusercontent.com/43415122/49168627-f9dceb80-f348-11e8-86a3-4886eb92ce44.png).

![pf_png](https://user-images.githubusercontent.com/43415122/50472460-8ec31b00-09c1-11e9-874d-c6b0afa123e8.png)


 **Алгоритм**
 
*Вход*:Обучающая выборка из l пар «объект-ответ» ![default](https://user-images.githubusercontent.com/43415122/49168732-36a8e280-f349-11e8-8ce2-c8332bb410d4.png)

*Выход*: Значения параметров ![default](https://user-images.githubusercontent.com/43415122/49168800-5fc97300-f349-11e8-9b9b-561b29388fd6.png) для всех ![default](https://user-images.githubusercontent.com/43415122/49168978-cd759f00-f349-11e8-8874-b16173bdc53b.png).

*Описание алгоритма*:
1. Инициализация: ![default](https://user-images.githubusercontent.com/43415122/49169037-f85ff300-f349-11e8-8c91-bb32fb2ab2b7.png)для всех ![default](https://user-images.githubusercontent.com/43415122/49169069-0ada2c80-f34a-11e8-88f0-034964a62a80.png).
2. Повторять пункты 3-4, пока ![default](https://user-images.githubusercontent.com/43415122/49169127-2cd3af00-f34a-11e8-90c3-8d41a5fa8635.png)(то есть пока процесс не стабилизируется):
 3.  Выбрать очередной объект x_i из выборки;
 4.  Если ![default](https://user-images.githubusercontent.com/43415122/49169229-673d4c00-f34a-11e8-8fc8-7e2ce002a164.png), то ![default](https://user-images.githubusercontent.com/43415122/49169291-86d47480-f34a-11e8-8764-d6ccc3b0d41a.png).
 
 5.  Вернуть значение ![default](https://user-images.githubusercontent.com/43415122/49169353-a23f7f80-f34a-11e8-90ba-523f6134ee1b.png) для всех ![default](https://user-images.githubusercontent.com/43415122/49169402-b8e5d680-f34a-11e8-801e-4af1469ab30a.png).
 
 6.  Пока число ошибок на выборке не окажется достаточно мало.

**Достоинства**:
 - Метод прост для понимания и алгоритмической реализации;
 - Порождает потоковый алгоритм;
 - Хранит лишь часть выборки, следовательно, экономит память.
 
 **Недостатки**:
 - Порождаемый алгоритм медленно сходится
 
 
 # Сравнение алгоритмов классификации

 <table>
  <tr>
    <th>Метод</th>
    <th>Параметры</th>
    <th>Величина ошибок</th>
  </tr>
  <tr>
    <td>KNN</td>
    <td>k=6</td>
    <td>0.033</td>
  </tr>
  <tr>
    <td>KWNN</td>
    <td>k=9</td>
    <td>0.033</td>
  </tr>
  <tr>
    <td>PW,Гауссовское ядро</td>
    <td>h=0.1</td>
    <td>0.04</td>
  </tr>
  <td>PW,ядро Епанечникова</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Треугольное ядро</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Квартическое ядро</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Прямоугольное ядро</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
</table>
 
 
 
 # Байесовские классификаторы
 
 Байесовские алгоритмы классификации основаны на предположении, что есть вероятностное пространство X x Y с неизвестной плотностью распределения ![68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f25354372686f2532302532387825324325323079253239253230253344253230502532387925323925354372686f2532302532387825323025374325323079253239](https://user-images.githubusercontent.com/43415122/49568481-c3e7ca80-f939-11e8-86a8-6ec878a71f67.gif)
, из которого случайно и независимо извлекаются l наблюдений.

Байесовский подход опирается на теорему о том, что *если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.*

Обозначим величину потери алгоритмом **а** при неправильной классификации объекта класса **y** .

**Теорема**: Если известны априорные вероятности классов P(y) и функции правдоподобия p(x|y), то минимум среднего риска достигается алгоритмом ![yutyg](https://user-images.githubusercontent.com/43415122/49568554-fc87a400-f939-11e8-8726-9ec6aafe5eab.gif)
 Алгоритм **a(x)** называется оптимальным *байесовским решающим правилом.*

На практике зачастую плотности распределения классов неизвестны и их приходится восстанавливать по обучающей выборке. *Чем лучше удастся восстановить функции правдоподобия, тем ближе к оптимальному будет построенный алгоритм.*

В зависимости от способов восстановления плотности существует большое разнообразие байесовских алгоритмов классификации.

# Наивный нормальный байесовский классификатор

Пусть все признаки независимы и нормально распределены с мат.ожиданием ![y](https://user-images.githubusercontent.com/43415122/50269761-092af280-0438-11e9-852c-df148ef6dbd9.gif) и дисперсией ![k](https://user-images.githubusercontent.com/43415122/50269792-2790ee00-0438-11e9-83bb-cb319ea5aed5.gif) отличающимися для разных классов:

![c6e](https://user-images.githubusercontent.com/43415122/50269719-dd0f7180-0437-11e9-8694-3e67ed014bca.gif)

Тогда ковариационные матрицы и их выборочные оценки будут диагональны.
В этом случае проблемы вырожденности  и мультиколлинеарности не возникают.
Метод обучения до крайности прост и сводится к вычислению параметров ![y](https://user-images.githubusercontent.com/43415122/50269761-092af280-0438-11e9-852c-df148ef6dbd9.gif) и ![k](https://user-images.githubusercontent.com/43415122/50269792-2790ee00-0438-11e9-83bb-cb319ea5aed5.gif) для всех и всех признаков.

По каждому признаку отдельно в каждом классе высчитаем матожидание и дисперсию и подставив в плотность получим:

![default](https://user-images.githubusercontent.com/43415122/51758999-3c683d80-20d8-11e9-8027-6be0b39773a6.png)

где <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\mu&space;_{yj}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\mu&space;_{yj}}" title="\widehat{\mu _{yj}}" /></a>  и <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\sigma&space;_{yj}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\sigma&space;_{yj}}" title="\widehat{\sigma _{yj}}" /></a> - оценки матожидания и дисперсии j-го признака, вычисленные по <a href="https://www.codecogs.com/eqnedit.php?latex=X_{y}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?X_{y}" title="X_{y}" /></a> - подвыборке класса y.

Результат классификации классов, имеющих разные матрицы ковариации и карта классификации всех объектов: 

![naive_all 1](https://user-images.githubusercontent.com/43415122/51761817-1db97500-20df-11e9-93f4-8865b837eca0.png)



# Линии уровня нормального распределения

Это специальный случай баесовской классификации, когда предполагается, что плотности всех классов являются многомерными нормальными. В этом случае задача решается аналитически. Сами плотности вычисляются по формуле:
![2018-12-06 11-59-40_cut-photo ru](https://user-images.githubusercontent.com/43415122/49570479-5e96d800-f93f-11e8-83b7-481fd33d2db5.png)
в которой 

X -объект, состоящий из n признаков,

![2018-12-06 11-59-40_cut-photo ru 1](https://user-images.githubusercontent.com/43415122/49570598-c2b99c00-f93f-11e8-8c55-0331f700cfed.png) -математическое ожидание,

![2018-12-06 11-59-40_cut-photo ru 2](https://user-images.githubusercontent.com/43415122/49570618-d402a880-f93f-11e8-9572-982a276440f1.png) -ковариационная матрица.


**Примеры для таких центров и ковариационных матриц:**


 1)mu = (0, 0), cov = ![2018-12-13 11-35-32_cut-photo ru](https://user-images.githubusercontent.com/43415122/49922981-da9e9c00-feba-11e8-81d5-b369ff9b0844.png)
 
                       
![photofacefun_com_1544082445](https://user-images.githubusercontent.com/43415122/49569212-feeafd80-f93b-11e8-97c3-edaedebffee7.jpg)

 2)mu = (0, 0), cov = ![2018-12-13 11-40-13_cut-photo ru](https://user-images.githubusercontent.com/43415122/49923136-3c5f0600-febb-11e8-9251-ab26d0a3de04.png)

 
![photofacefun_com_1544082939](https://user-images.githubusercontent.com/43415122/49569622-1c6c9700-f93d-11e8-908a-e9190decbcc9.jpg)

3)mu = (0, 0), cov = ![2018-12-13 11-40-13_cut-photo ru 1](https://user-images.githubusercontent.com/43415122/49923162-4aad2200-febb-11e8-98e2-863bc5c38155.png)


![photofacefun_com_1544082684](https://user-images.githubusercontent.com/43415122/49569411-89336180-f93c-11e8-98d1-da7b27903020.jpg)

4)mu = (0, 0), cov = ![2018-12-13 11-40-13_cut-photo ru 2](https://user-images.githubusercontent.com/43415122/49923176-5a2c6b00-febb-11e8-8bf0-f1c7deb70493.png)


![photofacefun_com_1544082826](https://user-images.githubusercontent.com/43415122/49569524-d9aabf00-f93c-11e8-85f9-17f491115cb6.jpg)

Реализация в Shiny

![default](https://user-images.githubusercontent.com/43415122/50457228-1a5d8d00-096b-11e9-9617-890145a2aed0.png)

![default](https://user-images.githubusercontent.com/43415122/50457231-25182200-096b-11e9-9ee9-5b40b3a351a9.png)

![default](https://user-images.githubusercontent.com/43415122/50457234-2c3f3000-096b-11e9-9da6-dda4c9299024.png)


# Подстановочный алгоритм (Plug-in)

Оценим параметры функций правдоподобия <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\mu&space;_{y}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\mu&space;_{y}}" title="\widehat{\mu _{y}}" /></a> и <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\Sigma_{y}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\Sigma_{y}}" title="\widehat{\Sigma_{y}}" /></a> по частям обучающей выборки <a href="https://www.codecogs.com/eqnedit.php?latex=\chi&space;_{y}^{l}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\chi&space;_{y}^{l}" title="\chi _{y}^{l}" /></a> для каждого класса y <a href="https://www.codecogs.com/eqnedit.php?latex=\in" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\in" title="\in" /></a> Y. Затем эти выборочные оценки подставим в оптимальный байесовский классификатор. Получим байесовский нормальный классификатор,который называется также *подстановочным*.

В асимптотике <a href="https://www.codecogs.com/eqnedit.php?latex=l_{y}&space;\rightarrow&space;\propto" target="_blank"><img src="https://latex.codecogs.com/gif.latex?l_{y}&space;\rightarrow&space;\propto" title="l_{y} \rightarrow \propto" /></a> оценки  <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\mu&space;_{y}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\mu&space;_{y}}" title="\widehat{\mu _{y}}" /></a> и <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\Sigma_{y}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\Sigma_{y}}" title="\widehat{\Sigma_{y}}" /></a> обладают рядом оптимальных свойств: они **не смещены, состоятельны и эффективны**. Однако оценки,сделанные по коротким выборкам,могут быть не достаточно точными.

**Недостатки** *подстановочного алгоритма* вытекают из нескольких чрезмерно сильных базовых предположений,которые на практике часто не выполняются.

 - Функции правдоподобия классов могут существенно отличаться от гауссовских.В частности, когда имеются признак,принимающие дискретные значени или когда классы распадаются на изолированные сгустки.
 - Если длина выборки меньше размерности пространства или среди признаков есть линейно зависимые, то матрица становится вырожденной. В этом случае обратная матрица не существет и метод вообще неприменим.
 - Выборочные оценки чувствительны к нарушениям нормальности распределений, в частности,  к редким большим выбросам.

**Некоторые примеры:**

1.Если признаки некоррелированы, линии уровня плотности распределения имеют форму элипсоидов с центром в точке <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\mu&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\mu&space;}" title="\widehat{\mu }" /></a>  и параллельны осям координат.

![1](https://user-images.githubusercontent.com/43415122/51692025-132eab00-200d-11e9-9962-742231c4d4cd.png)



2.Если признаки имею одинаковые дисперсии, то элипсоиды являются сферами

![2](https://user-images.githubusercontent.com/43415122/51692048-217cc700-200d-11e9-802a-719ddb98739a.png)

3.Пример, когда менее плотный класс окружен более плотным

![3](https://user-images.githubusercontent.com/43415122/51692066-2b062f00-200d-11e9-9566-8cd3e0b0e234.png)

# Линейный дискриминант Фишера

Пусть ковариационные матрицы классов одинаковы и равны <a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma" title="\Sigma" /></a>.  Оценим <a href="https://www.codecogs.com/eqnedit.php?latex=\widehat{\Sigma}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\widehat{\Sigma}" title="\widehat{\Sigma}" /></a> по всем m объектам выборки. С учетом поправки на смещенность, 

![default](https://user-images.githubusercontent.com/43415122/51693977-f7c59f00-2010-11e9-9642-40b6fd97a399.png)

В этом случае разделяющая поверхность линейна (кусочно-линейна). Подстановочный алгоритм имеет вид : 
<a href="https://www.codecogs.com/eqnedit.php?latex=a(x)=arg\max_{y&space;\in&space;Y}&space;\lambda&space;_{y}P&space;_{y}\rho&space;_{y}(x)=&space;arg\max_{y&space;\in&space;Y}(ln(\lambda&space;_{y}P_{y})-\frac{1}{2}&space;\widehat{\mu}_{y}^{\top&space;}\Sigma&space;^{-1}\widehat{\mu&space;}_{y}&plus;x^{\top&space;}\Sigma&space;^{-1}\widehat{\mu&space;_{y}})=arg\max_{y&space;\in&space;Y}(x^{\top&space;}\alpha&space;_{y}&plus;\beta&space;_{y})." target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x)=arg\max_{y&space;\in&space;Y}&space;\lambda&space;_{y}P&space;_{y}\rho&space;_{y}(x)=&space;arg\max_{y&space;\in&space;Y}(ln(\lambda&space;_{y}P_{y})-\frac{1}{2}&space;\widehat{\mu}_{y}^{\top&space;}\Sigma&space;^{-1}\widehat{\mu&space;}_{y}&plus;x^{\top&space;}\Sigma&space;^{-1}\widehat{\mu&space;_{y}})=arg\max_{y&space;\in&space;Y}(x^{\top&space;}\alpha&space;_{y}&plus;\beta&space;_{y})." title="a(x)=arg\max_{y \in Y} \lambda _{y}P _{y}\rho _{y}(x)= arg\max_{y \in Y}(ln(\lambda _{y}P_{y})-\frac{1}{2} \widehat{\mu}_{y}^{\top }\Sigma ^{-1}\widehat{\mu }_{y}+x^{\top }\Sigma ^{-1}\widehat{\mu _{y}})=arg\max_{y \in Y}(x^{\top }\alpha _{y}+\beta _{y})." /></a>

Этот алгоритм называется линейным дискриминантом Фишера (ЛДФ). 

Он неплохо работает, когда формы классов действительно близки к норамльным и не слишком сильно различаются. в этом случае линейное решающее парвило близко к оптиамльному байесовскому, но существенно более устойчиво, чем квадратичное, и часто обладает лучшей обобщающей способностью.

**Примеры:**

1.Дисперсии признаков обоих классов одинаковы:
![1](https://user-images.githubusercontent.com/43415122/51696495-b1733e80-2016-11e9-98ec-dfc737cb3275.png)

2.Одинаковые дисперсии,небольшое кол-во объектов в классах:
![2](https://user-images.githubusercontent.com/43415122/51696634-fc8d5180-2016-11e9-8b9a-37b29fbb8742.png)

3.Разные дисперсии:
![3](https://user-images.githubusercontent.com/43415122/51696669-0fa02180-2017-11e9-9ab1-49c57cc39832.png)


 # ЕМ-алгоритм

Класс не во всех случаях можно описать одним распределением. Иногда класс представляет собой смесь распределений. В этом случае функция правдоподобия p(x) вычисляется по формуле:

![default](https://user-images.githubusercontent.com/43415122/51762159-029b3500-20e0-11e9-8dc9-487c45777b76.png)

где j - номер компоненты смеси, <a href="https://www.codecogs.com/eqnedit.php?latex=w_{j}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w_{j}" title="w_{j}" /></a> - её апостериорная вероятность. Задача разделения смеси состоит в том, чтобы имея обучающую выборку, выделить из неё компоненты смеси, и оценить вектор параметров ![default](https://user-images.githubusercontent.com/43415122/51762223-32e2d380-20e0-11e9-920b-d594454ab019.png).

Для разделения смеси применяется EM-алгоритм. Он состоит из двух больших шагов: E (expectation) - шага и M (maximization) - шага. В начале алгоритма задаётся начальное приближение <a href="https://www.codecogs.com/eqnedit.php?latex=\theta" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\theta" title="\theta" /></a>, затем на E - шаге по <a href="https://www.codecogs.com/eqnedit.php?latex=\theta" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\theta" title="\theta" /></a> вычисляется вектор скрытых переменных G. На M - шаге по текущим значениям G и <a href="https://www.codecogs.com/eqnedit.php?latex=\theta" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\theta" title="\theta" /></a> вычисляется новое значение <a href="https://www.codecogs.com/eqnedit.php?latex=\theta" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\theta" title="\theta" /></a>. Процесс повторяется, пока G и <a href="https://www.codecogs.com/eqnedit.php?latex=\theta" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\theta" title="\theta" /></a> не стабилизируются.



# Линейные классификаторы

Пусть ![3](https://user-images.githubusercontent.com/43415122/51696876-981ec200-2017-11e9-8c7a-bfe558ba35ca.png) и ![2](https://user-images.githubusercontent.com/43415122/51696890-a1a82a00-2017-11e9-9e93-9a446bb53f11.png).

Алгоритм ![2](https://user-images.githubusercontent.com/43415122/51696927-ba184480-2017-11e9-8316-74cab1cc91ea.png) является линейным алгоритмом классификации. Если f>0, то алгоритм a относит x к классу +1. Иначе к классу -1.

Уравнение ![2](https://user-images.githubusercontent.com/43415122/51697003-dc11c700-2017-11e9-8520-5dc741000885.png) называется уравнением разделяющей поверхности.

Величина ![2](https://user-images.githubusercontent.com/43415122/51697172-3c086d80-2018-11e9-9aaf-3b784b64cf26.png) называется отступом объекта относительно алгоритма классификации. Если ![2](https://user-images.githubusercontent.com/43415122/51697271-7b36be80-2018-11e9-8e0a-0b2bccc6c9eb.png) алгоритм совершает на объекте ![2](https://user-images.githubusercontent.com/43415122/51697303-8ee22500-2018-11e9-9705-5edb794e9580.png)  ошибку.

![2](https://user-images.githubusercontent.com/43415122/51697331-9d304100-2018-11e9-833e-31b6256aa582.png)- монотонно невозрастающая функция потерь, мажорирует пороговую функцию ![2](https://user-images.githubusercontent.com/43415122/51697379-b507c500-2018-11e9-91c6-981f17484b4e.png). Тогда минимизацю суммарных потерь можно рассматривать как функцию вида ![2](https://user-images.githubusercontent.com/43415122/51697403-c4870e00-2018-11e9-91da-0431219b63fd.png).




 # Метод стохастического градиента
 
 Для минимизации Q(w) применяется метод градиентного спуска. В начале выбирается некоторое начальное приближение вектора весов w. Инициализация весов может производиться разными способами. Стандартная рекомендация- взять небольшие случайные значения: <a href="https://www.codecogs.com/eqnedit.php?latex=w_{j}=random(-\frac{1}{2n},\frac{1}{2n})" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w_{j}=random(-\frac{1}{2n},\frac{1}{2n})" title="w_{j}=random(-\frac{1}{2n},\frac{1}{2n})" /></a>, где n – количество признаков x.
 Далее высчитывается текущая оценка функционала: ![2](https://user-images.githubusercontent.com/43415122/51698145-87238000-201a-11e9-84bc-30fa0d685e21.png)
 
 Затем запускается итерационный процесс, на каждом шаге которого вектор w изменяется в сторону наиболее быстрого убывания Q. Это направление противоположно вектору градиента ![2](https://user-images.githubusercontent.com/43415122/51698198-a6baa880-201a-11e9-8753-2c036c0eda52.png). Соответственно веса меняются по правилу:
 ![2](https://user-images.githubusercontent.com/43415122/51698225-b89c4b80-201a-11e9-9516-137134440fe4.png)
где <a href="https://www.codecogs.com/eqnedit.php?latex=\eta" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\eta" title="\eta" /></a> – темп обучения. 
 
 **Алгоритм:**
 
 *Вход*: ![m](https://user-images.githubusercontent.com/43415122/50270575-aa1aad00-043a-11e9-9264-414e26e6292c.gif) -выборка,
        ![default](https://user-images.githubusercontent.com/43415122/50270629-de8e6900-043a-11e9-898d-186b5a5003bc.gif) - темп обучения,![default](https://user-images.githubusercontent.com/43415122/50270653-f82fb080-043a-11e9-81fe-b78acf28330a.gif)-параметр сглаживания.
        
 *Выход*: вектор весов w.

  1.Инициализировать веса 
  
  2.Вычислить начальное значение функционала ![default](https://user-images.githubusercontent.com/43415122/50270741-43e25a00-043b-11e9-8047-e3bee52b85ca.gif)
 
 3.Повторить
 
 4.Выбрать объект ![default](https://user-images.githubusercontent.com/43415122/50270774-61172880-043b-11e9-8a33-57e279aa1cb9.gif) из ![default](https://user-images.githubusercontent.com/43415122/50270808-7ab87000-043b-11e9-81c8-bf0bfea0f753.gif).
 
 5.Вычислить ошибку алгоритма ![default](https://user-images.githubusercontent.com/43415122/50270845-9459b780-043b-11e9-9e62-4e34865c8f6e.gif)
 
 6.Сделать шаг градиентного спуска ![default](https://user-images.githubusercontent.com/43415122/50271247-dcc5a500-043c-11e9-87dd-0425f8ceabb0.gif)
 
 7.Оценить новоее значение функционала ![default](https://user-images.githubusercontent.com/43415122/50271291-f666ec80-043c-11e9-84c1-e21ac637a250.gif)
 
 8.Пока значение Q не стабилизировалось и/или веса w не перестанут изменяться.
 
 **Преимущества**:
 
 - Метод легко реализуется и обобщается на нелинейные классификаторы и на нейронные сети- суперпозиция линейных классификаторов.
 - Метод позволяет настраивать веса на избыточно больших выборках.
 - Метод подходит для динамического обучения, когда обучающие объекты поступают потоком и вектор весов обновляется при появлении каждого объекта.
 
 **Недостатки**:
 
 - Функционал Q многоэкстремальный и процесс может сходиться к локальному минимуму,может сходиться очень медленно или не сходиться вовсе.
 - При большой размерности пространства или малой длине выборки возможно переобучение.
 - Если функция потерь имеет горизонтальные ассимптоты, то процесс может попасть в состояние "паралича".

 # ADALINE
 
   ADALINE - адаптивный линейный элемент, в качестве функции потерь используется квадратичная функция потерь:
 ![k](https://user-images.githubusercontent.com/43415122/50470736-7ac7eb00-09ba-11e9-86e6-0dc5fa49354f.gif)
  
  Градиентный шаг - дельта-правило ![default](https://user-images.githubusercontent.com/43415122/50470827-dabe9180-09ba-11e9-99f1-32aaea4aaa3f.gif)
  
Обучение ADALINE с помощью стохастического градиента:

```diff
 sg.ADALINE <- function(xl, eta = 1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  
  # инициализация Q
  Q <- 0
  for (i in 1:l)
  {
   
    wx <- sum(w * xl[i, 1:n]) # скалярное произведение <w,x>
    
    margin <- wx * xl[i, n + 1] # отступ
    
    Q <- Q + lossQuad(margin)
  }
  
  repeat
  {
    #  выч отступ для все объектов обучаемой выборки
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      margins[i] <- crossprod(w, xi) * yi
    }
    
    
    errorIndexes <- which(margins <= 0) # выбор ошибочных объектов
    
    if (length(errorIndexes) > 0)
    {
      # случайный выбор индексов из ошибок
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      
      wx <- sum(w * xi)# скалярноe произведения <w,xi>
      
      
      margin <- wx * yi # производится шаг градиента
      
      # вычисление ошибок
      ex <- lossQuad(margin)
      eta <- 1 / sqrt(sum(xi * xi))
      w <- w - eta * (wx - yi) * xi
      
      # вычисление нового Q
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
    }
    else
    {
      break
    }
  }
  return (w)
}
```

Примеры:



1.Выборки можно разделить прямой
![2](https://user-images.githubusercontent.com/43415122/51710134-38d1a980-2039-11e9-8af5-d5d80d11a00f.png)


2.Невозможно разделить прямой
![2](https://user-images.githubusercontent.com/43415122/51709947-daa4c680-2038-11e9-9de8-c036cea94f3d.png)





# Логистическая регрессия

Данный классификатор также можно назвать оптимальным байесовским.

Имеет логистическую функцию потерь  ![2](https://user-images.githubusercontent.com/43415122/51708865-06727d00-2036-11e9-9524-b431d218e221.png) и логистическое правило обновления весов ![2](https://user-images.githubusercontent.com/43415122/51708890-18542000-2036-11e9-9584-87d9238b2c7d.png), где ![2](https://user-images.githubusercontent.com/43415122/51708931-2efa7700-2036-11e9-84c3-53c9ed15abb9.png)-сигмоидная функция.

Примеры:

1.Выборки можно разделить прямой
![2](https://user-images.githubusercontent.com/43415122/51710300-bd242c80-2039-11e9-85a1-a29716341b3c.png)

2.Невозможно разделить прямой
![2](https://user-images.githubusercontent.com/43415122/51710254-98c85000-2039-11e9-8d0f-bd876d00fa09.png)






# Персептрон Розенблатта

Линейный алгоритм классификации, основанный на методе стохастического градиента с кусочно-линейной функцией потерь: 
![2](https://user-images.githubusercontent.com/43415122/51709100-9dd7d000-2036-11e9-9c47-1bfe8b0924a8.png)
Также меняется правило обновления весов:
![default](https://user-images.githubusercontent.com/43415122/51773248-4487a380-20ff-11e9-8dcb-8a4808361afe.png)


Примеры:

1.Выборки можно точно разделить прямой
![2](https://user-images.githubusercontent.com/43415122/51709726-3cb0fc00-2038-11e9-9890-9c2763f62adf.png)

2.Выборки невозможно разделить прямой
![2](https://user-images.githubusercontent.com/43415122/51709814-784bc600-2038-11e9-8e1c-e5568538ff29.png)

# Метод опорных векторов

**SVM** - алгоритм классификации, считающийся одним из самых лучших. Он назван так, потому что положение разделяющей классы гиперплоскости зависит лишь от небольшого количества элементов обучающей выборки. Они и называются опорными векторами.

С помощью функции ядра метод обобщается на случай нелинейных разделяющих поверхностей. По сути вид поверхности зависит от используемой функции ядра.

Если выборка линейно разделима, то почти всегда провести разделяющую поверхность можно не единственным образом. В **SVM** она выбирается таким образом, чтобы она отстояла максимально далеко от обоих классов. Умножим алгоритм a(x) на некоторую константу так, чтобы минимальный отступ в каждом классе был равен единице. Тогда полосу, разделяющую классы описывает множество точек: 

![default](https://user-images.githubusercontent.com/43415122/51762457-d3d18e80-20e0-11e9-8be4-c636670579fb.png)

Границами полосы служат две гиперплоскости, на которых лежат объекты с минимальным отступом. Ширина полосы вычисляется по формуле: 

![default](https://user-images.githubusercontent.com/43415122/51762503-fa8fc500-20e0-11e9-8373-ce0bbd91ec1e.png)


Для линейно неразделимой выборки разрешим алгоритму допускать некоторое число ошибок при классификации (т. е. разрешим объектам попадать внутрь разделяющей полосы). Для этого введём дополнительные переменные <a href="https://www.codecogs.com/eqnedit.php?latex=\xi&space;_{i}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\xi&space;_{i}" title="\xi _{i}" /></a>, означающие величину ошибки на объекте i. Тогда выражение ![default](https://user-images.githubusercontent.com/43415122/51762579-2dd25400-20e1-11e9-8fb0-b5070e444f36.png)
 (мы хотим минимизировать норму вектора w) превратится в  ![default](https://user-images.githubusercontent.com/43415122/51762637-4e9aa980-20e1-11e9-81e0-103da34b07a7.png), где C - управляющий параметр, позволяющий находить компромисс между максимизацией ширины полосы и минимизацией суммарной ошибки.






