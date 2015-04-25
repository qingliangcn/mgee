# mgee
2010年写的一个erlang游戏服务端demo

## 说明

1. 代码未作美化或者修改，仅仅作为一个记录，代码质量不做任何保证
2. 部分文件可能会涉及到开源版权问题，请自行处理[当年无知，改了开源文件的版权内容]
3. 其他八卦见这里:[为什么一些网页游戏喜欢用Erlang做服务端？](http://www.zhihu.com/question/20405300/answer/45747560?group_id=575056971175112704)
4. 本demo离商业化差距太大(当年是边学边写)，如直接使用，本人概不负责

## 代码结构

目录|说明
:----|:------
config|配置文件，目前只有技能和地图(前端用)
doc|当时写的一些文档
doc/设计文档|设计思路都在这里
doc/message|协议说明
doc/tech|技术问题的记录
include|头文件目录
proto|proto定义目录
scripts|脚本目录
src|源码目录
src/mod|游戏模块
src/library|第三方模块
src/proto|第三方erlang proto库
test|压力测试用
unittest|单元测试

## 编译运行

### Mac/Linux

进入源码根目录

```
make proto && make all
sudo mgectl live # live方式启动，需要回车一次继续
sudo mgectl start # 后台方式启动
```
PS:需要sudo权限的原因是服务端监听了843端口(安全沙箱)

以live模式启动正常的话可以看到类似如下的结果输出:

```
starting Test Data Setup                          ...done

system running :)

=PROGRESS REPORT==== 25-Apr-2015::16:00:57 ===
         application: mgee
          started_at: mgee@localhost
```

### Windows

1. 进入源码目录的scripts目录
2. 运行rebuild_proto.cmd
3. 运行make.cmd
4. 运行run.cmd

## 需要的工具

1. Erlang运行时环境
2. make支持(mac/linux)
3. 阅读设计文档:Enterprise Archtect (eap格式文件)

