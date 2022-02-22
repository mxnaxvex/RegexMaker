# RegexMaker
一种从数据中提取正则表达式的算法

## 测试示例1

~~~
    val data = "国药准字Z22020031\n国药准字Z44021605\n国药准字H20052134\n国药准字H20065128\n" +
      "国药准字H20063321\n国药准字Z45020047\n国药准字Z44023696\n国药准字Z44020041\n" +
      "国药准字B20020729\n国药准字Z37021536\n国药准字Z10970003\n国药准字Z20025893\n" +
      "国药准字Z20025021\n国药准字Z41021818\n国药准字Z13021046\n国药准字Z10980075\n" +
      "国药准字Z22020445\n国药准字Z45021256\n国药准字Z20027946\n国药准字Z20027945\n" +
      "国药准字Z20054513\n国药准字Z20064282\n国药准字Z20064281\n国药准字Z20063129\n" +
      "国药准字Z20053531"
    val reg = new RegTree
    reg.addData(data.split("\n"))
    // 加两条脏数据
    reg.addData("J20030052")
      .addData("国药准字")
    reg.mine
    println(reg.getRegString())
~~~

输出结果
~~~
国药准字[A-Z]\d{8}
~~~

## 测试示例2
~~~
    val reg = new RegTree
    //3个字母+3个数字 系列
    reg.addData("abc123", 5)
      .addData("cdd334", 3)
      .addData("fdk324", 8)
      .addData("odv082", 1)
      .addData("jdv492", 3)
      .addData("mfd729", 5)
      .addData("uepw158", 1)
      .addData("ids491", 1)
    //4个字母+3个数字 系列
    reg.addData("kkkk653", 20)
    //11位数字 系列
    reg.addData("13567891234", 20)
      .addData("13812345678", 20)
    //脏数据
    reg.addData("ovd4321", 1)
      .addData("123456789012345")

    val r = reg.mine.mkString("\n\n")
    println(r)

~~~
输出结果
~~~
\d{11}|[a-z]{3,4}\d{3}
~~~
