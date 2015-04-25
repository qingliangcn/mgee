%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-16
%%% @doc all network msg packet define at this,
%%%		and use php script auto generated mgee_vo.erl module.
%%% @end
%%%----------------------------------------------------------------------

%% =================== ERROR msg 	===================
-record(m_result_fail, {result=fail, reason}).

%% =================== module: login 	===================
% method: flash_login
-record(m_login_flash_login_recv, {account_name, account_pwd} ).
-record(m_login_flash_login_send, {result=ok, rolelist} ).

%% =================== module: role		===================
% method: list
-record(m_role_list_recv, {} ).
-record(m_role_list_send, {result=ok, rolelist} ).

% method: add
-record(m_role_add_recv, {rolename, sex, skinid} ).
-record(m_role_add_send, {result=ok, rolelist} ).

% method: del
-record(m_role_del_recv, {roleid } ).
-record(m_role_del_send, {result=ok, rolelist} ).

% method: enter
-record(m_role_enter_recv, {roleid} ).
-record(m_role_enter_send, {result=ok, role_attr} ).

%% =================== module: vw		===================
% method: enter
-record(m_vw_enter_recv, {vwid} ).
-record(m_vw_enter_send, {result=ok, rolelist} ).
-record(m_vw_enter_send2, {roleid, role_attr} ).

%% =================== module: move		===================
% method: pos_sync
% not recv
-record(m_move_pos_sync_send, { sync_list } ).
% sync_list is a list: { roleid, dir, px, py, tx, ty }

% method: walk_path
-record(m_move_walk_path_recv, { p } ).
-record(m_move_walk_path_send, {result, reason} ).
-record(m_move_walk_path_send2, {roleid, p } ).
%% End with number 2 means send this msg to other rolelist, except self.

% method: walk
-record(m_move_walk_recv, { dir, px, py, tx, ty } ).
-record(m_move_walk_send, {result, reason} ).
%-record(m_move_walk_send2, { roleid, dir, px, py, tx, ty } ).

% method: run
-record(m_move_run_recv, { dir, px, py, tx, ty } ).
-record(m_move_run_send, {result, reason} ).
%-record(m_move_run_send2, { roleid, dir, px, py, tx, ty } ).

%% =================== module: chat		===================
% method: world
-record(m_chat_world_recv, {body} ).
-record(m_chat_world_send, {result=ok, reason} ).
-record(m_chat_world_send2, {roleid, rolename, body } ).

% method: private
-record(m_chat_private_recv, {to_roleid, to_rolename, body } ).
-record(m_chat_private_send, {result=ok, reason} ).
-record(m_chat_private_send2, {roleid, rolename, body } ).

% method: view
-record(m_chat_view_recv, {body} ).
-record(m_chat_view_send, {result=ok, reason} ).
-record(m_chat_view_send2, {roleid, rolename, body } ).

% method: team
-record(m_chat_team_recv, {body} ).
-record(m_chat_team_send, {result=ok, reason} ).
-record(m_chat_team_send2, {roleid, rolename, body } ).

% method: family
-record(m_chat_family_recv, {body} ).
-record(m_chat_family_send, {result=ok, reason} ).
-record(m_chat_family_send2, {roleid, rolename, body } ).

% method: faction
-record(m_chat_faction_recv, {body} ).
-record(m_chat_faction_send, {result=ok, reason} ).
-record(m_chat_faction_send2, {roleid, rolename, body } ).

%% =================== module: fight		===================



