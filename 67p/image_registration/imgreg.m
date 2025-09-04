clear all;close all;clc;

files = dir('*.jpg');
n = length(files);

shift = zeros(n/2,3);
cor_result = zeros(n/2,1);

for name = 1:2:n
    num = (name+1)/2
    display(' processing.....')
    name_str = files(name).name(1:25);
    simu_name = strcat(name_str,'_simu.jpg');
    img_name = strcat(name_str,'_image.jpg');
    simu = imread(simu_name);
    img  = imread(img_name);

    %img  = img * 100;
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % 去边  上下左右各去除？个像素
    %img = img(6:size(img,1)-5,6:size(img,2)-5);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % 将过大的值置零，避免影响计算  处理img数据
%     for i = 1:size(img,1)
%         for j = 1:size(img,2)
%             if img (i,j) > 1 || img(i,j) < -1
%                 img(i,j) = 0;
%                 %         else if img(i,j) > 0.1
%                 %                 img(i,j) = 1;
%                 %             end
%             end
%         end
%     end

    % 去除数据中NaN的值
    img(isnan(img)==1) = 0;
    fake(isnan(fake)==1) = 0;
   
    
%     
%     figure
%     subplot(1,2,1)
%     imshow(img);
%     subplot(1,2,2)
%     imshow(fake)

    % 将fake扩展至2倍大小
    fake_exp = zeros(size(fake,1)*2,size(fake,2)*2);
    fake_exp(size(fake,1)/2+1:size(fake,1)/2*3 , size(fake,2)/2+1:size(fake,2)/2*3) = fake;
    A = fake_exp;
    B = img;
    
    [row,col] = size(A);      % A的尺寸
    [row_r,col_r] = size(B);  % B的尺寸
    steps = 20;              % 向上向下个移动20步，查找最大值
    C = zeros(2*steps+1);         % 结果存储矩阵
    
    for j = -steps:1:steps
        for i = -steps:1:steps
            % 在A中取B大小的区域，与B卷积，从A中心偏左上  -steps/2  到右下  steps/2
            sec_A = A(floor(row/2) - floor(row_r/2) + i + 1:floor(row/2) + floor(row_r/2) + i  , ...
                floor(col/2) - floor(col_r/2) + j + 1:floor(col/2) + floor(col_r/2) + j );
            C(i + steps + 1 , j + steps + 1) = corr2(sec_A,B);
        end
    end
    max_C = max(max(C));
    [max_row,max_col] = find(C == max_C);
    
    
%     D = zeros(21);
%     for j = -10:10
%         for i = -10:10
%             % 在A中取B大小的区域，与B卷积，从A中心偏左上  -steps/2  到右下  steps/2
%             sec_A = A(floor(row/2) - floor(row_r/2) + max_row - steps/2 - 1 + i + 1:floor(row/2) + floor(row_r/2) + max_row - steps/2 -1 + i , ...
%                 floor(col/2) - floor(col_r/2) + max_col - steps/2 - 1 + j + 1:floor(col/2) + floor(col_r/2) + max_col - steps/2 -1 + j );
%             D(i + 10 + 1 , j + 10 + 1) = corr2(sec_A,B);
%         end
%     end
%     max_D = max(max(D));
%     [max_row1,max_col1] = find(D == max_D);
%     cor_result(name,1) = max_D;
    
    cor_A = A(floor(row/2) - floor(row_r/2) + (max_row -steps -1) + 1:floor(row/2) + floor(row_r/2) + (max_row -steps/2 -1) , ...
              floor(col/2) - floor(col_r/2) + (max_col -steps -1) + 1:floor(col/2) + floor(col_r/2) + (max_col -steps/2 -1) );
    % figure
    % imshow(cor_A);
    %
    % figure
    % imshow((cor_A + B) / 2);
    
%     cor_A1 = A(floor(row/2) - floor(row_r/2) + max_row -steps/2 - 1 + max_row1 -11 + 1:floor(row/2) + floor(row_r/2) + max_row -steps/2 - 1 + max_row1 -11, ...
%         floor(col/2) - floor(col_r/2) + max_col -steps/2 - 1 + max_col1 -11 + 1:floor(col/2) + floor(col_r/2) + max_col -steps/2 - 1 + max_col1 -11);
    
%     figure
%     imshow(cor_A1);
    figure(num)
    subplot(2,2,1);
    imshow(img);title('img');
    subplot(2,2,2)
    imshow(cor_A);title('cut');
    subplot(2,2,3)
    imshow(fake);title('fake')
    subplot(2,2,4)
    imshow((cor_A + B)/2);
   
    
%     shift_row = max_row - steps/2 - 1 + max_row1 - 11;
%     shift_col = max_col - steps/2 - 1 + max_row1 - 11;
    
    shift_row = max_row - steps/2 - 1 ;
    shift_col = max_col - steps/2 - 1 ;
    
    shift(num,1) = mean(shift_row);
    shift(num,2) = mean(shift_col);
    
    display('Finished.')
end




