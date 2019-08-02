function [retval] = cmet(input1, input2, t)

% NOTE: This function assumes normalized inputs (values between 0 and 1)

% Fetch dimensions
[x, y] = size(input2);

% Initialize counter
accum = 0.0;

% Calculate deviance
for i = [1:x]
  for j = [1:y]
    if abs(input2(i, j) - input1(i, j)) < t
      accum = accum + 1 / (x * y);
    end
  end
end

[retval] = accum;

endfunction
