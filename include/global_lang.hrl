%%%----------------------------------------------------------------------
%%% File    : global_lang.hrl
%%% Author  : odinxu
%%% Created : 2010-01-08
%%% Description: Language constant
%%%				多国语言支持，只需要修改这个文件就行。
%%%				程序中所有需要直接使用字符串（特别是中文）输出结果的，
%%%				务必得设置成这个文件里的常量。
%%%	
%%% 命名规定 :  1. 必须全部以 _LANG_ 开头
%%%			   2. 紧接着是 模块名
%%%			   3. 接着是操作名
%%%			   4. 最后是成功 OK， 失败 FAIL，跟着的是原因
%%%----------------------------------------------------------------------


%%----------------------  通用的     ----------------------
-define(_LANG_PARAM_ERROR,					<<"参数错误">>							).

%%----------------------  登录系统  ----------------------

-define(_LANG_LOGIN_WRONG_PWD, <<"密码错误！">>).

%%----------------------  账号系统  ----------------------

-define(_LANG_ACCOUNT_NOT_EXISTS, 			<<"账号不存在！">>).
-define(_LANG_ACCOUNT_NAME_EXISTS, 			<<"账号名已存在！">>).
-define(_LANG_ACCOUNT_EMPTY, 				<<"账号名不能为空！">>).

%%----------------------  角色系统  ----------------------

-define(_LANG_ROLE_MAX_COUNT_LIMIT, <<"创建角色数量已达系统限制！">>).
-define(_LANG_ROLE_NAME_EXIST, <<"角色名已存在！">>).
-define(_LANG_ROLE_NOT_EXISTS, <<"角色不存在！">>).

%%----------------------  好友系统  ----------------------
-define(_LANG_FRIEND_ADD_OK, 				<<"成功把对方加为好友.">>). 
-define(_LANG_FRIEND_ADD_FAIL_MAX_LIMIT,	<<"您的好友数已达上限.">>).
-define(_LANG_FRIEND_ADD_REFUSE, 			<<"对方拒绝加为好友.">>). 
-define(_LANG_FRIEND_ADD_FAIL_BAD, 			<<"失败,对方已把你加入了黑名单.">>).
-define(_LANG_FRIEND_ADD_FAIL_REPEAT,		<<"对方已经是您的好友.">>).

-define(_LANG_FRIEND_NOT_EXIST,				<<"对方已不在你的好友列表.">>). 

-define(_LANG_FRIEND_DEL_OK, 				<<"成功把对方从好友列表删除.">>). 

-define(_LANG_FRIEND_REMARK_OK, 			<<"好友备注更新成功.">>).
-define(_LANG_FRIEND_REMARK_NOT_CHANGE,		<<"好友备注没有改动,不用更新.">>).

-define(_LANG_FRIEND_GROUP_OK, 				<<"好友分组更新成功.">>).


%%----------------------  聊天系统  ----------------------

-define(_LANG_CHAT_TOO_FAST, 				<<"发言间隔时间小于3秒！">>).


%%----------------------  战斗系统  ----------------------
-define(_LANG_FIGHT_NOT_IN_ATTACK_RANGE, 	<<"不在攻击范围内.">>).



%%----------------------  组队系统  ----------------------
-define(_LANG_TEAM_NOT_EXISTS,				<<"未创建队伍.">>	 					).
-define(_LANG_TEAM_NOT_IN,					<<"你不在队伍中.">>	 					).
-define(_LANG_TEAM_CREATE_FAIL,				<<"创建队伍失败.">>	 					).
-define(_LANG_TEAM_INVITE_FAIL_OFFLINE,		<<"邀请失败,对方不在线.">>				).
-define(_LANG_TEAM_INVITE_FAIL_REPEAT,		<<"对方已经在队伍中,不需要重复邀请.">>	).
-define(_LANG_TEAM_INVITE_FAIL_EXIST,		<<"邀请失败,对方有队伍了.">>				).
-define(_LANG_TEAM_ACCEPT_FAIL_NOT_INVITE,	<<"加入队伍失败,对方没有邀请你加入.">>	).
-define(_LANG_TEAM_ACCEPT_REPEAT,			<<"你已经在队伍中,不需要重复加入.">>	 	).
-define(_LANG_TEAM_ACCEPT_FAIL_MAX_LIMIT,	<<"加入队伍失败,该队伍人数已满.">>		).
-define(_LANG_TEAM_ACCEPT_FAIL_EXIT_OLD_TEAM,	<<"加入队伍失败,未能成功退出旧队伍.">>	).
-define(_LANG_TEAM_LEAVE_FAIL_NOT_IN,		<<"你已经不在队伍中.">>	 				).
-define(_LANG_TEAM_LEAVE_FAIL_IS_LEADER,	<<"你是队长,不能离开队伍(此功能未实现)">> ).
-define(_LANG_TEAM_LEADER_AUTHORITY,		<<"对不起,这个只有队长才能操作.">>	 	).
-define(_LANG_TEAM_REFUSE_FAIL_NOT_INVITE,	<<"拒绝失败,对方没有邀请你加入.">>		).
-define(_LANG_TEAM_FOLLOW_FAIL_IS_LEADER,	<<"跟随失败,队长不能自己跟随自己.">>		).
-define(_LANG_TEAM_FOLLOW_FAIL_REPEAT,		<<"请不需要重复设置跟随操作.">>			).
-define(_LANG_TEAM_FOLLOW_FAIL_TOO_FAR,		<<"跟随失败,跟队长距离太远.">>			).
-define(_LANG_TEAM_CHANGE_LEADER_FAIL_ONLY_ONE,		<<"队伍只剩下一个人.">>			).
-define(_LANG_TEAM_CHANGE_LEADER_FAIL_NOT_IN2,		<<"被转让的人不在队伍中.">>		).
-define(_LANG_TEAM_CHANGE_LEADER_FAIL_TO_SELF,		<<"不能转让给自己.">>			).	
-define(_LANG_TEAM_KICK_FAIL_NOT_IN,		<<"踢人失败,对方不在队伍内.">>			).
-define(_LANG_TEAM_KICK_FAIL_SELF,			<<"踢人失败,不能踢自己.">>				).

%%----------------------  行走系统  ----------------------
-define(_LANG_WALK_CANNOT_PASS,				<<"该点不可行走！">>).


%%----------------------  xx系统  ----------------------