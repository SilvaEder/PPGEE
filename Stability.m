%*****************************************************************
% Entrada de dados 
%*****************************************************************
clear all, close all, clc

J=4566068.16/1000000; % Kg*m^2
d=283/1000;           % m
dg=2.92/1000;         % m
P=281.37/1000;        % Kg
g=9.81;               % m/s^2

A=[0 1; (-dg*g*P)/J 0];
B=[0;d/J];
C=[1 0];
D=[0];

%% *****************************************************************
%
%*****************************************************************
W=sdpvar(2,2,'sym');
Z=sdpvar(1,2,'full');
LMIs=[W >= 0, W*A' + Z'*B' + A*W + B*Z <= 0];
solvesdp(LMIs,[]);
[primal,dual] = checkset(LMIs);
if (primal >= 0)
    K = double(Z)*inv(double(W));
    fprintf(' Matriz de Ganho \n');
    fprintf('\n');
    disp(K);
    fprintf('\n');
    fprintf(' Autovalores de (A+B*K) \n');
    fprintf('\n');
    disp(eig(A+B*K));
    SimulationRobust(K)
else
    disp('Infactivel');
end
fprintf('\n');
