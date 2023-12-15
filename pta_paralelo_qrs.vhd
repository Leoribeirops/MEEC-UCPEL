LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY MULT_GEN IS
    GENERIC (N : INTEGER);
    PORT (
        A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        Y : OUT STD_LOGIC_VECTOR(2 * N - 1 DOWNTO 0)
    );
END MULT_GEN;
ARCHITECTURE COMP OF MULT_GEN IS
BEGIN
    Y <= A * B;
END COMP;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY SUM_GEN IS
    GENERIC (N : INTEGER := 32);
    PORT (
        A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        Y : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END SUM_GEN;
ARCHITECTURE COMP OF SUM_GEN IS
BEGIN
    Y <= A + B;
END COMP;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY SUM_GEN_2 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        Y : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END SUM_GEN_2;
ARCHITECTURE COMP OF SUM_GEN_2 IS
BEGIN
    Y <= A + B;
END COMP;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

ENTITY REG_GEN IS
    GENERIC (N : INTEGER);
    PORT (
        clock, LD, CL : IN STD_LOGIC;
        A : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        S : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
END REG_GEN;

ARCHITECTURE COMP OF REG_GEN IS
    SIGNAL MS : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
BEGIN
    PROCESS (clock, CL)
    BEGIN
        IF CL = '1' THEN
            MS <= (OTHERS => '0');
        ELSIF (clock'event AND clock = '1') THEN
            IF LD = '1' THEN
                MS <= A;
            ELSE
                MS <= MS;
            END IF;
        END IF;
        S <= MS;
    END PROCESS;
END COMP;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY REG_GEN_INT IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clock, LD, CL : IN STD_LOGIC;
        A : IN INTEGER RANGE -2 ** 17 TO 2 ** 17 - 1;
        S : OUT INTEGER RANGE -2 ** 17 TO 2 ** 17 - 1
    );
END REG_GEN_INT;

ARCHITECTURE COMP OF REG_GEN_INT IS
    SIGNAL MS : INTEGER RANGE -2 ** 17 TO 2 ** 17 - 1;
BEGIN
    PROCESS (clock, CL)
    BEGIN
        IF CL = '1' THEN
            MS <= 0;
        ELSIF (clock'event AND clock = '1') THEN
            IF LD = '1' THEN
                MS <= A;
            ELSE
                MS <= MS;
            END IF;
        END IF;
        S <= MS;
    END PROCESS;
END COMP;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY calc IS
    GENERIC (N : INTEGER := 18);
    PORT (
        valor_arm, SIG_LEV : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        result : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END calc;

ARCHITECTURE behav OF calc IS
    SIGNAL valor_arm_int, SIG_LEV_int, result_int : INTEGER;
BEGIN
    -- Convertendo std_logic_vector para INTEGER
    valor_arm_int <= TO_INTEGER(UNSIGNED(valor_arm));
    SIG_LEV_int <= TO_INTEGER(UNSIGNED(SIG_LEV));

    PROCESS (valor_arm_int, SIG_LEV_int)
    BEGIN
        -- result_int = 0.125*valor_arm_int + 0.875*SIG_LEV_int;
        --result_int <= (valor_arm_int / 8) + (7 * SIG_LEV_int / 8);
        --0.25 * valor_arm e 0.75 * SIG_LEV)
        --result_int <= (valor_arm_int / 8) + (7 * SIG_LEV_int / 7);
        result_int <= (valor_arm_int / 4) + (3 * SIG_LEV_int / 4);
    END PROCESS;

    -- Convertendo INTEGER para std_logic_vector
    result <= STD_LOGIC_VECTOR(TO_UNSIGNED(result_int, N));
END behav;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY calc_m IS
    GENERIC (N : INTEGER := 18);
    PORT (
        valor_arm, SIG_LEV : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        result : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END calc_m;

ARCHITECTURE behav OF calc_m IS
    SIGNAL valor_arm_int, SIG_LEV_int, result_int : INTEGER;
BEGIN
    -- Convertendo std_logic_vector para INTEGER
    valor_arm_int <= TO_INTEGER(UNSIGNED(valor_arm));
    SIG_LEV_int <= TO_INTEGER(UNSIGNED(SIG_LEV));

    PROCESS (valor_arm_int, SIG_LEV_int)
    BEGIN
        -- Realizando o cÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¡lculo 0.125*valor_arm + 0.875*SIG_LEV 
        -- ou (valor_arm / 8) + 7 * (SIG_LEV / 8)
        -- Multiplicando por 7 e dividindo por 8 ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â© equivalente a 
        -- (SIG_LEV*8 - SIG_LEV) / 8 = SIG_LEV - (SIG_LEV / 8)
        -- result_int = 0.125*valor_arm_int + 0.875*SIG_LEV_int;
        --result_int <= (valor_arm_int / 8) + SIG_LEV_int - (SIG_LEV_int / 8);
        result_int <= (valor_arm_int / 8) + (7 * SIG_LEV_int / 8);
    END PROCESS;

    -- Convertendo INTEGER para std_logic_vector
    result <= STD_LOGIC_VECTOR(TO_UNSIGNED(result_int, N));
END behav;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY calc_3 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        valor_arm, SIG_LEV : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        result : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END calc_3;

ARCHITECTURE behav OF calc_3 IS
    SIGNAL valor_arm_int, SIG_LEV_int, result_int : INTEGER;
BEGIN
    -- Convertendo std_logic_vector para INTEGER
    valor_arm_int <= TO_INTEGER(UNSIGNED(valor_arm));
    SIG_LEV_int <= TO_INTEGER(UNSIGNED(SIG_LEV));

    PROCESS (valor_arm_int, SIG_LEV_int)
    BEGIN
        -- Realizando o cÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¡lculo 0.125*valor_arm + 0.875*SIG_LEV 
        -- ou (valor_arm / 8) + 7 * (SIG_LEV / 8)
        -- Multiplicando por 7 e dividindo por 8 ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â© equivalente a 
        -- (SIG_LEV*8 - SIG_LEV) / 8 = SIG_LEV - (SIG_LEV / 8)
        -- result_int = 0.125*valor_arm_int + 0.875*SIG_LEV_int;
        --result_int <= (valor_arm_int / 8) + SIG_LEV_int - (SIG_LEV_int / 8);
        result_int <= (valor_arm_int / 8) + (7 * SIG_LEV_int / 7);
        --result_int <= (valor_arm_int / 8) + (7 * SIG_LEV_int / 7);
    END PROCESS;

    -- Convertendo INTEGER para std_logic_vector
    result <= STD_LOGIC_VECTOR(TO_UNSIGNED(result_int, N));
END behav;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY TRUN_GEN_C IS
    GENERIC (N : INTEGER);
    PORT (
        A : IN STD_LOGIC_VECTOR(4 * N - 1 DOWNTO 0);
        Y : OUT STD_LOGIC_VECTOR(2 * N - 1 DOWNTO 0));
END TRUN_GEN_C;

ARCHITECTURE COMP7 OF TRUN_GEN_C IS
BEGIN
    --Y <= A(4*N-1)&A(3*N-4 DOWNTO N+6);--7
    Y <= A(4 * N - 1) & A(3 * N - 2 DOWNTO N);--8 funciona 1,50,25
    --Y <= A(4*N-1)&A(3*N-1 DOWNTO N+1);--9
    --Y <= A(4*N-1)&A(3*N DOWNTO N+2);--10
    --Y <= A(4*N-1)&A(3*N+1 DOWNTO N+3); --11
    --Y <= A(4*N-1)&A(3*N+2 DOWNTO N+4);--12  *
    --Y <= A(4*N-1)&A(3*N+3 DOWNTO N+5);--13
    --Y <= A(4*N-1)&A(3*N+4 DOWNTO N+6);--14
END COMP7;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY max_out IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        entrada : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        --new_data : in std_logic;
        saida : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END max_out;

ARCHITECTURE Behavioral OF max_out IS
    --type state_type is (idle, compare);
    --signal state : state_type;
    SIGNAL max_temp : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);

BEGIN
    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            --state <= idle;
            max_temp <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF max_temp < entrada THEN
                max_temp <= entrada;
                --else
                --max_temp <= max_temp;
            END IF;
        END IF;
    END PROCESS;
    saida <= max_temp;
END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY magnitude_comparator IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        A : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
        out_put : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0)
    );
END magnitude_comparator;

ARCHITECTURE Behavioral OF magnitude_comparator IS
    SIGNAL max_temp : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
BEGIN
    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            max_temp <= "100000000000000000"; -- menor valor para 18 bits em complemento de dois.
        ELSIF rising_edge(clk) THEN
            IF signed(A) > signed(max_temp) THEN
                max_temp <= A;
            ELSE
                max_temp <= max_temp;
            END IF;
        END IF;
    END PROCESS;
    out_put <= max_temp;
END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY MUX2_1_GEN IS
    GENERIC (N : INTEGER);
    PORT (
        A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        SEL : STD_LOGIC;
        Y : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END MUX2_1_GEN;
ARCHITECTURE COMP OF MUX2_1_GEN IS
BEGIN

    y <= A WHEN sel = '0' ELSE
        B WHEN sel = '1';

END COMP;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY MUX2_1_GEN_bit IS
    --GENERIC (N : INTEGER);
    PORT (
        A, B : IN STD_LOGIC;
        SEL : STD_LOGIC;
        Y : OUT STD_LOGIC
    );
END MUX2_1_GEN_bit;
ARCHITECTURE COMP OF MUX2_1_GEN_bit IS
BEGIN

    y <= A WHEN sel = '0' ELSE
        B WHEN sel = '1';

END COMP;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY memory_01 IS
    GENERIC (
        N : INTEGER := 18; -- Tamanho do vetor de dados
        M : INTEGER := 720 -- Tamanho da memória
    );
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        write_pointer : IN INTEGER;
        read_pointer : IN INTEGER;
        write_enable : IN STD_LOGIC;
        read_enable : IN STD_LOGIC
    );
END ENTITY memory_01;

ARCHITECTURE bh OF memory_01 IS
    TYPE memory IS ARRAY (0 TO M - 1) OF STD_LOGIC_VECTOR(N - 1 DOWNTO 0); -- Use M aqui
    SIGNAL mem : memory := (OTHERS => (OTHERS => '0'));

BEGIN
    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            mem <= (OTHERS => (OTHERS => '0'));
        ELSIF rising_edge(clk) THEN
            IF write_enable = '1' THEN
                mem(write_pointer) <= data_in;
            END IF;
            IF read_enable = '1' THEN
                data_out <= mem(read_pointer);
            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE bh;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY PulseGenerator IS
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        trigger : IN STD_LOGIC; -- Sinal de ativação
        pulse : OUT STD_LOGIC); -- Sinal de pulso
END PulseGenerator;

ARCHITECTURE Behavioral OF PulseGenerator IS
    TYPE State_Type IS (espera, pulso);
    SIGNAL state : State_Type := espera;
    SIGNAL pulse_reg : STD_LOGIC := '0';
    SIGNAL trigger_prev : STD_LOGIC := '0'; -- Registrador para o valor anterior do trigger
BEGIN

    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            state <= espera;
            pulse_reg <= '0';
            trigger_prev <= '0';
        ELSIF rising_edge(clk) THEN
            trigger_prev <= trigger; -- Amostra o valor atual do trigger
            CASE state IS
                WHEN espera =>
                    IF trigger = '1' AND trigger_prev = '0' THEN
                        -- Borda de subida detectada no trigger
                        state <= pulso;
                        pulse_reg <= '1';
                    ELSE
                        state <= espera;
                        pulse_reg <= '0';
                    END IF;
                WHEN pulso =>
                    state <= espera;
                    pulse_reg <= '0';
                WHEN OTHERS =>
                    state <= espera;
                    pulse_reg <= '0';
            END CASE;
        END IF;
    END PROCESS;

    pulse <= pulse_reg;

END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY MaxFinder IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;
        ecg_m : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
        max_flag : OUT STD_LOGIC;
        max_value : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
END MaxFinder;

ARCHITECTURE Behavioral OF MaxFinder IS
    SIGNAL max_internal : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL prev_max_internal : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL prev_ecg_m : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL flag_internal : STD_LOGIC := '0';
BEGIN
    PROCESS (clk, rst)
    BEGIN
        IF rst = '1' THEN
            max_internal <= (OTHERS => '0');
            prev_max_internal <= (OTHERS => '0');
            prev_ecg_m <= (OTHERS => '0');
            flag_internal <= '0';
        ELSIF rising_edge(clk) THEN
            IF ecg_m > max_internal THEN
                prev_max_internal <= max_internal; -- Store the previous max value
                max_internal <= ecg_m; -- Update the max value
                flag_internal <= '0'; -- Reset the flag if a new max is found
            ELSIF prev_ecg_m = prev_max_internal AND ecg_m < prev_ecg_m THEN
                flag_internal <= '1'; -- Set the flag if a peak is detected
            ELSE
                flag_internal <= '0'; -- Reset the flag in all other cases
            END IF;
            prev_ecg_m <= ecg_m; -- Store the current value for the next clock cycle
        END IF;
    END PROCESS;

    max_value <= max_internal;
    max_flag <= flag_internal;
END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY MaxFinder_2 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;
        ecg_m : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
        max_flag : OUT STD_LOGIC;
        max_value : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
END MaxFinder_2;

ARCHITECTURE Behavioral OF MaxFinder_2 IS
    CONSTANT threshold : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := "000000011000000000"; -- Value of 3 in fixed point representation
    SIGNAL max_internal : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL prev_max_internal : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL prev_ecg_m : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL flag_internal : STD_LOGIC := '0';
BEGIN
    PROCESS (clk, rst)
    BEGIN
        IF rst = '1' THEN
            max_internal <= (OTHERS => '0');
            prev_max_internal <= (OTHERS => '0');
            prev_ecg_m <= (OTHERS => '0');
            flag_internal <= '0';
        ELSIF rising_edge(clk) THEN
            IF ecg_m >= threshold THEN -- Check if ecg_m is greater than or equal to 3
                IF ecg_m > max_internal THEN
                    prev_max_internal <= max_internal; -- Store the previous max value
                    max_internal <= ecg_m; -- Update the max value
                    flag_internal <= '0'; -- Reset the flag if a new max is found
                ELSIF prev_ecg_m = prev_max_internal AND ecg_m < prev_ecg_m THEN
                    flag_internal <= '1'; -- Set the flag if a peak is detected
                ELSE
                    flag_internal <= '0'; -- Reset the flag in all other cases
                END IF;
                prev_ecg_m <= ecg_m; -- Store the current value for the next clock cycle
            ELSE
                flag_internal <= '0'; -- Reset the flag if ecg_m is below the threshold
            END IF;
        END IF;
    END PROCESS;

    max_value <= max_internal;
    max_flag <= flag_internal;
END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY MaxFinder_3 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;
        ecg_m : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
        max_flag : OUT STD_LOGIC;
        max_value : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
END MaxFinder_3;

ARCHITECTURE Behavioral OF MaxFinder_3 IS
    --CONSTANT threshold : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := "000000011000000000"; -- Threshold value 3
    CONSTANT threshold : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := "000000000010011001"; -- Threshold value 0,3 paralelo
    --CONSTANT threshold : STD_LOGIC_VECTOR (N DOWNTO 0) := "000000000000001111"; -- Threshold value 0.03 sequencial
    SIGNAL max_internal : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL prev_ecg_m : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL flag_internal : STD_LOGIC := '0';
    SIGNAL potential_max : BOOLEAN := FALSE;
BEGIN
    PROCESS (clk, rst)
    BEGIN
        IF rst = '1' THEN
            max_internal <= (OTHERS => '0');
            prev_ecg_m <= (OTHERS => '0');
            flag_internal <= '0';
            potential_max <= FALSE;
        ELSIF rising_edge(clk) THEN
            IF ecg_m >= threshold THEN
                IF ecg_m > max_internal THEN
                    max_internal <= ecg_m;
                    potential_max <= TRUE;
                    flag_internal <= '0';
                END IF;
            ELSE
                IF potential_max = TRUE AND max_internal > prev_ecg_m THEN
                    flag_internal <= '1';
                ELSE
                    flag_internal <= '0';
                END IF;
                potential_max <= FALSE;
                max_internal <= (OTHERS => '0');
            END IF;
            prev_ecg_m <= ecg_m;
        END IF;
    END PROCESS;

    max_value <= max_internal;
    max_flag <= flag_internal;
END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY edge_detector IS
    GENERIC (
        NUM_CYCLES : INTEGER := 10 -- Valor padrão do contador
    );
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC; -- Sinal de reset adicionado
        entrada : IN STD_LOGIC;
        controle : OUT STD_LOGIC -- Sinal de controle como saída
    );
END edge_detector;

ARCHITECTURE Behavioral OF edge_detector IS

    TYPE state_type IS (idle, waiting_for_edge, counting, generate_pulse);
    SIGNAL state : state_type := idle;
    SIGNAL counter : INTEGER RANGE 0 TO NUM_CYCLES := 0;
    SIGNAL entrada_prev : STD_LOGIC := '1';
    SIGNAL controle_interno : STD_LOGIC := '0'; -- Sinal interno para controle

BEGIN

    -- Atribui o valor interno ao sinal de saída controle
    controle <= controle_interno;

    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            -- Reseta todos os estados e sinais para os valores iniciais
            state <= idle;
            counter <= 0;
            controle_interno <= '0';
            entrada_prev <= '1';
        ELSIF rising_edge(clk) THEN
            -- Lógica de detecção de borda e contagem
            CASE state IS
                WHEN idle =>
                    -- Sistema no estado ocioso, aguardando por borda de descida
                    IF entrada_prev = '1' AND entrada = '0' THEN
                        -- Borda de descida detectada, inicia a contagem
                        state <= counting;
                        counter <= 1; -- Inicia o contador
                        -- Não atualiza entrada_prev até que o pulso tenha sido gerado e o estado resetado
                    END IF;
                WHEN counting =>
                    -- Conta NUM_CYCLES ciclos de clock após a detecção
                    IF counter < NUM_CYCLES THEN
                        counter <= counter + 1; -- Incrementa o contador
                    ELSE
                        -- Contagem completa, gera pulso no sinal de controle
                        controle_interno <= '1';
                        state <= generate_pulse; -- Transita para o estado de gerar pulso
                    END IF;
                WHEN generate_pulse =>
                    -- Gera o pulso e retorna ao estado de esperar pela borda
                    controle_interno <= '0'; -- Reseta o sinal de controle
                    state <= waiting_for_edge; -- Muda para o estado de esperar pela próxima borda
                    counter <= 0; -- Reseta o contador
                WHEN waiting_for_edge =>
                    -- Espera que o sinal de entrada volte para '1' para estar pronto para a próxima borda de descida
                    IF entrada = '1' THEN
                        state <= idle;
                        entrada_prev <= '1'; -- Reseta o valor anterior de entrada
                    END IF;
            END CASE;
        END IF;
    END PROCESS;

END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY edge_detector_2 IS
    GENERIC (
        NUM_CYCLES : INTEGER := 10; -- Valor padrão do contador para detecção de borda
        PULSE_WIDTH : INTEGER := 5 -- Valor padrão para a largura do pulso de controle
    );
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC; -- Sinal de reset adicionado
        entrada : IN STD_LOGIC;
        controle : OUT STD_LOGIC -- Sinal de controle como saída
    );
END edge_detector_2;

ARCHITECTURE Behavioral OF edge_detector_2 IS

    TYPE state_type IS (idle, waiting_for_edge, counting, generate_pulse, pulse_on);
    SIGNAL state : state_type := idle;
    SIGNAL counter : INTEGER RANGE 0 TO NUM_CYCLES := 0;
    SIGNAL pulse_counter : INTEGER RANGE 0 TO PULSE_WIDTH := 0; -- Contador para a largura do pulso
    SIGNAL entrada_prev : STD_LOGIC := '1';
    SIGNAL controle_interno : STD_LOGIC := '0'; -- Sinal interno para controle

BEGIN

    -- Atribui o valor interno ao sinal de saída controle
    controle <= controle_interno;

    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            -- Reseta todos os estados e sinais para os valores iniciais
            state <= idle;
            counter <= 0;
            pulse_counter <= 0; -- Reseta também o contador do pulso
            controle_interno <= '0';
            entrada_prev <= '1';
        ELSIF rising_edge(clk) THEN
            -- Lógica de detecção de borda, contagem e geração de pulso
            CASE state IS
                WHEN idle =>
                    -- Sistema no estado ocioso, aguardando por borda de descida
                    IF entrada_prev = '1' AND entrada = '0' THEN
                        -- Borda de descida detectada, inicia a contagem
                        state <= counting;
                        counter <= 1; -- Inicia o contador
                    END IF;
                WHEN counting =>
                    -- Conta NUM_CYCLES ciclos de clock após a detecção
                    IF counter < NUM_CYCLES THEN
                        counter <= counter + 1; -- Incrementa o contador
                    ELSE
                        -- Contagem completa, inicia o pulso no sinal de controle
                        controle_interno <= '1';
                        state <= pulse_on; -- Entra no estado de pulso ativo
                        pulse_counter <= 1; -- Inicia o contador da largura do pulso
                    END IF;
                WHEN pulse_on =>
                    -- Conta PULSE_WIDTH ciclos de clock enquanto o pulso está ativo
                    IF pulse_counter < PULSE_WIDTH THEN
                        pulse_counter <= pulse_counter + 1; -- Incrementa o contador do pulso
                    ELSE
                        -- Largura do pulso completa, desativa o pulso
                        controle_interno <= '0';
                        state <= waiting_for_edge; -- Retorna ao estado de espera pela próxima borda
                    END IF;
                WHEN generate_pulse =>
                    -- Estado removido, já que a geração do pulso agora é controlada por PULSE_WIDTH
                WHEN waiting_for_edge =>
                    -- Espera que o sinal de entrada volte para '1' para estar pronto para a próxima borda de descida
                    IF entrada = '1' THEN
                        state <= idle;
                        entrada_prev <= '1'; -- Reseta o valor anterior de entrada
                    END IF;
            END CASE;
        END IF;
    END PROCESS;

END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

-- Declaração da entidade
ENTITY Demux1Bit2Out IS
    PORT (
        input : IN STD_LOGIC;
        SEL : IN STD_LOGIC;
        output1 : OUT STD_LOGIC;
        output2 : OUT STD_LOGIC);
END Demux1Bit2Out;

-- Declaração da arquitetura
ARCHITECTURE Behavioral OF Demux1Bit2Out IS
BEGIN
    -- Processo que define o comportamento do demultiplexador
    PROCESS (input, SEL)
    BEGIN
        IF SEL = '0' THEN
            output1 <= input;
            output2 <= '0'; -- Desativa a outra saída
        ELSE
            output1 <= '0'; -- Desativa esta saída
            output2 <= input;
        END IF;
    END PROCESS;
END Behavioral;

LIBRARY ieee;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY twoscompliment IS
    GENERIC (N : INTEGER := 32);
    PORT (
        --Inputs
        A : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
        --Outputs
        Y : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0)
    );
END twoscompliment;

ARCHITECTURE COMP OF twoscompliment IS
    SIGNAL A_aux : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
BEGIN
    A_aux <= (NOT A);
    Y <= A_aux + '1';
END COMP;
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE comp_gen IS

    COMPONENT MULT_GEN IS
        GENERIC (N : INTEGER);
        PORT (
            A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            Y : OUT STD_LOGIC_VECTOR(2 * N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT SUM_GEN IS
        GENERIC (N : INTEGER := 36);
        --GENERIC (N: INTEGER);
        PORT (
            A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            Y : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT SUM_GEN_2 IS
        GENERIC (N : INTEGER := 18);
        --GENERIC (N: INTEGER);
        PORT (
            A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            Y : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT REG_GEN IS
        GENERIC (N : INTEGER);
        PORT (
            clock, LD, CL : IN STD_LOGIC;
            A : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            S : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
    END COMPONENT;

    COMPONENT REG_GEN_INT IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clock, LD, CL : IN STD_LOGIC;
            A : IN INTEGER RANGE -2 ** 17 TO 2 ** 17 - 1;
            S : OUT INTEGER RANGE -2 ** 17 TO 2 ** 17 - 1
        );
    END COMPONENT;

    COMPONENT calc IS
        GENERIC (N : INTEGER := 18);
        PORT (
            valor_arm, SIG_LEV : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            result : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT calc_m IS
        GENERIC (N : INTEGER := 18);
        PORT (
            valor_arm, SIG_LEV : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            result : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT calc_3 IS
        GENERIC (N : INTEGER := 18);
        PORT (
            valor_arm, SIG_LEV : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            result : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT TRUN_GEN_C IS
        GENERIC (N : INTEGER);
        PORT (
            A : IN STD_LOGIC_VECTOR(4 * N - 1 DOWNTO 0);
            Y : OUT STD_LOGIC_VECTOR(2 * N - 1 DOWNTO 0));
    END COMPONENT;

    COMPONENT max_out IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            entrada : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            --new_data : in std_logic;
            saida : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT magnitude_comparator IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            A : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
            out_put : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT MUX2_1_GEN IS
        GENERIC (N : INTEGER);
        PORT (
            A, B : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            SEL : STD_LOGIC;
            Y : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT MUX2_1_GEN_bit IS
        PORT (
            A, B : IN STD_LOGIC;
            SEL : STD_LOGIC;
            Y : OUT STD_LOGIC
        );
    END COMPONENT;

    COMPONENT memory_01 IS
        GENERIC (
            N : INTEGER := 18; -- Tamanho do vetor de dados
            M : INTEGER := 720 -- Tamanho da memória
        );
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            data_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            write_pointer : IN INTEGER;
            read_pointer : IN INTEGER;
            write_enable : IN STD_LOGIC;
            read_enable : IN STD_LOGIC
        );
    END COMPONENT;

    COMPONENT PulseGenerator IS
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            trigger : IN STD_LOGIC; -- Sinal de ativação
            pulse : OUT STD_LOGIC); -- Sinal de pulso
    END COMPONENT;

    COMPONENT MaxFinder IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            rst : IN STD_LOGIC;
            ecg_m : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
            max_flag : OUT STD_LOGIC;
            max_value : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
    END COMPONENT;

    COMPONENT MaxFinder_2 IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            rst : IN STD_LOGIC;
            ecg_m : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
            max_flag : OUT STD_LOGIC;
            max_value : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
    END COMPONENT;

    COMPONENT MaxFinder_3 IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            rst : IN STD_LOGIC;
            ecg_m : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
            max_flag : OUT STD_LOGIC;
            max_value : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0));
    END COMPONENT;
    COMPONENT edge_detector IS
        GENERIC (
            NUM_CYCLES : INTEGER := 10 -- Valor padrão do contador
        );
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC; -- Sinal de reset adicionado
            entrada : IN STD_LOGIC;
            controle : OUT STD_LOGIC -- Sinal de controle como saída
        );
    END COMPONENT;

    COMPONENT edge_detector_2 IS
        GENERIC (
            NUM_CYCLES : INTEGER := 10; -- Valor padrão do contador para detecção de borda
            PULSE_WIDTH : INTEGER := 5 -- Valor padrão para a largura do pulso de controle
        );
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC; -- Sinal de reset adicionado
            entrada : IN STD_LOGIC;
            controle : OUT STD_LOGIC -- Sinal de controle como saída
        );
    END COMPONENT;

    COMPONENT Demux1Bit2Out IS
        PORT (
            input : IN STD_LOGIC;
            SEL : IN STD_LOGIC;
            output1 : OUT STD_LOGIC;
            output2 : OUT STD_LOGIC);
    END COMPONENT;

    COMPONENT twoscompliment IS
        GENERIC (N : INTEGER := 32);
        PORT (
            --Inputs
            A : IN STD_LOGIC_VECTOR (N - 1 DOWNTO 0);
            --Outputs
            Y : OUT STD_LOGIC_VECTOR (N - 1 DOWNTO 0)
        );
    END COMPONENT;
END comp_gen;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_arith.ALL;
USE work.comp_gen.ALL;

ENTITY Meancalculator_2 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        reset, Ld : IN STD_LOGIC;
        data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        mean_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END ENTITY Meancalculator_2;

ARCHITECTURE Structural OF Meancalculator_2 IS
    SIGNAL sum, out_soma : STD_LOGIC_VECTOR(2 * N - 1 DOWNTO 0);
    SIGNAL temp_product, temp_product2 : STD_LOGIC_VECTOR(2 * N - 1 DOWNTO 0);
    SIGNAL temp_1, sum80_0 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    CONSTANT factor_binary : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := "000000000000000001";
    --constant percent80 : STD_LOGIC_VECTOR (N-1 downto 0) := "000000000110011001";
BEGIN

    mult_0 : MULT_GEN GENERIC MAP(N) PORT MAP(data_in, factor_binary, temp_product);
    sum_0 : SUM_GEN GENERIC MAP(2 * N) PORT MAP(sum, temp_product, out_soma);
    reg : REG_GEN GENERIC MAP(2 * N) PORT MAP(clk, Ld, reset, out_soma, sum);
    trunc_0 : TRUN_GEN_C GENERIC MAP(N/2) PORT MAP(sum, temp_1);
    mean_out <= temp_1;
    --mult_1: MULT_GEN generic map(N) port map(temp_1, percent80, temp_product2); 
    --trunc_1: TRUN_GEN_C generic map(N/2) port map(temp_product2, mean_out80); 

END ARCHITECTURE Structural;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_arith.ALL;
USE work.comp_gen.ALL;

ENTITY percent80 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        --clk : in std_logic;
        --reset, Ld : in std_logic;
        data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END ENTITY percent80;

ARCHITECTURE Structural OF percent80 IS
    CONSTANT percent80 : STD_LOGIC_VECTOR (N - 1 DOWNTO 0) := "000000000110011001";
    SIGNAL temp : STD_LOGIC_VECTOR (2 * N - 1 DOWNTO 0);
BEGIN
    mult_0 : MULT_GEN GENERIC MAP(N) PORT MAP(data_in, percent80, temp);
    trun_0 : TRUN_GEN_C GENERIC MAP(N/2) PORT MAP(temp, data_out);
END ARCHITECTURE Structural;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.comp_gen.ALL;

ENTITY sub_gen_25p IS
    GENERIC (N : INTEGER := 18);
    PORT (
        IN_1, IN_2 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        sub_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END sub_gen_25p;

ARCHITECTURE behavioral OF sub_gen_25p IS
    SIGNAL temp_diff, reg_diff : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
BEGIN
    reg : REG_GEN GENERIC MAP(N) PORT MAP(clk, clk, reset, temp_diff, reg_diff);

    PROCESS (IN_1, IN_2)
        VARIABLE diff : signed(N - 1 DOWNTO 0);

    BEGIN
        -- ConversÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â£o dos sinais de entrada para signed
        IF to_integer(signed(IN_1)) >= to_integer(signed(IN_2)) THEN
            diff := signed(IN_1) - signed(IN_2);
        ELSE
            diff := signed(IN_2) - signed(IN_1);
        END IF;

        -- ConversÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â£o de volta para std_logic_vector para a saÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â­da
        --sub_out <= std_logic_vector(diff);
        temp_diff <= STD_LOGIC_VECTOR(diff);
        --sub_out <= temp_diff(N - 1) & temp_diff(N - 1) & temp_diff(N - 1 DOWNTO 2);
        sub_out <= reg_diff(N - 1) & reg_diff(N - 1) & reg_diff(N - 1 DOWNTO 2);

    END PROCESS;
END behavioral;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.comp_gen.ALL;

ENTITY sub_gen_125p IS -- Alterando o nome para refletir a nova constante
    GENERIC (N : INTEGER := 18);
    PORT (
        IN_1, IN_2 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        sub_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END sub_gen_125p;

ARCHITECTURE behavioral OF sub_gen_125p IS
    SIGNAL temp_diff, reg_diff : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL diff_scaled : signed(N - 1 DOWNTO 0); -- Adicionando um sinal para a diferença escalada
BEGIN
    reg : REG_GEN GENERIC MAP(N) PORT MAP(clk, clk, reset, temp_diff, reg_diff);

    PROCESS (IN_1, IN_2)
        VARIABLE diff : signed(N - 1 DOWNTO 0);

    BEGIN
        IF to_integer(signed(IN_1)) >= to_integer(signed(IN_2)) THEN
            diff := signed(IN_1) - signed(IN_2);
        ELSE
            diff := signed(IN_2) - signed(IN_1);
        END IF;

        diff_scaled <= diff / 8; -- Escalando a diferença por 0.125 (que é o mesmo que dividir por 8)

        temp_diff <= STD_LOGIC_VECTOR(diff_scaled);
        sub_out <= reg_diff(N - 1) & reg_diff(N - 1) & reg_diff(N - 1 DOWNTO 2);

    END PROCESS;
END behavioral;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.comp_gen.ALL;

ENTITY sub_gen_125p_2 IS -- Alterando o nome para refletir a nova constante
    GENERIC (N : INTEGER := 18);
    PORT (
        IN_1, IN_2 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        sub_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END sub_gen_125p_2;

ARCHITECTURE behavioral OF sub_gen_125p_2 IS
    SIGNAL temp_diff, reg_diff : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL diff_scaled, shifted_diff : signed(N DOWNTO 0); -- Aumentando em 1 bit para evitar overflow
BEGIN
    reg : REG_GEN GENERIC MAP(N) PORT MAP(clk, clk, reset, temp_diff, reg_diff);

    PROCESS (IN_1, IN_2)
        VARIABLE diff : signed(N - 1 DOWNTO 0);

    BEGIN
        IF to_integer(signed(IN_1)) >= to_integer(signed(IN_2)) THEN
            diff := signed(IN_1) - signed(IN_2);
        ELSE
            diff := signed(IN_2) - signed(IN_1);
        END IF;

        diff_scaled <= diff / 8; -- Escalando a diferença por 0.125 (que é o mesmo que dividir por 8)

        -- Fazendo o deslocamento para a esquerda para dobrar o valor
        shifted_diff <= resize(diff_scaled, N) SLL 1;

        -- Truncando o valor deslocado para manter em 18 bits
        temp_diff <= STD_LOGIC_VECTOR(shifted_diff(N - 1 DOWNTO 0));
        sub_out <= temp_diff;

    END PROCESS;
END behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.comp_gen.ALL;
--USE work.comp_gen2.ALL;
ENTITY ring_buffer_max IS -- ring_buffer_max
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        data_out : OUT INTEGER RANGE 0 TO 55; -- Modificado de out para buffer
        write_enable : IN STD_LOGIC;
        read_enable : IN STD_LOGIC;
        read_data : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END ENTITY ring_buffer_max;

ARCHITECTURE Behavioral OF ring_buffer_max IS
    SIGNAL write_pointer : INTEGER := 0;
    SIGNAL read_pointer : INTEGER := 0;
    SIGNAL position : INTEGER;
    SIGNAL data_temp : STD_LOGIC_VECTOR(N - 1 DOWNTO 0); -- Novo sinal intermediário
    SIGNAL valor_max_janela : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL max_temp : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);

    -- Variável para contar o número de ciclos de clock em que valor_max_janela não mudou
    SIGNAL stable_counter : INTEGER := 0;
    -- Variável para armazenar o valor anterior de valor_max_janela
    SIGNAL last_valor_max_janela : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL count_internal : INTEGER := 54;
    COMPONENT memory_01 IS
        GENERIC (
            N : INTEGER := 18; -- Tamanho do vetor de dados
            M : INTEGER := 54 -- Tamanho da memória
        );
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            data_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            write_pointer : IN INTEGER;
            read_pointer : IN INTEGER;
            write_enable : IN STD_LOGIC;
            read_enable : IN STD_LOGIC
        );
    END COMPONENT;
BEGIN

    mem_inst : memory_01
    PORT MAP(clk, reset, ecg_h, data_temp, write_pointer, read_pointer, write_enable, read_enable); -- Modificado para data_temp
    --max: magnitude_comparator_position GENERIC MAP(N) PORT MAP(clk, reset, data_temp, valor_max_janela, position); 
    max : magnitude_comparator GENERIC MAP(N) PORT MAP(clk, reset, data_temp, valor_max_janela);

    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN

            write_pointer <= 0;
            read_pointer <= 0;
        ELSIF rising_edge(clk) THEN
            IF write_enable = '1' THEN
                write_pointer <= (write_pointer + 1) MOD 54;
            END IF;
            IF read_enable = '1' THEN
                read_pointer <= (read_pointer + 1) MOD 54;
            END IF;
            -- Adicionado condição para resetar write_pointer se maior ou igual a 54
            IF write_pointer >= 54 THEN
                write_pointer <= 0;
            END IF;

        END IF;
    END PROCESS;

    PROCESS (clk)
    BEGIN
        IF rising_edge(clk) THEN
            IF read_enable = '1' THEN
                read_data <= data_temp; -- Modificado para data_temp
            END IF;
        END IF;
    END PROCESS;
    monitor_valor_max_janela : PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            max_temp <= "100000000000000000"; -- menor valor para 18 bits em complemento de dois.
            count_internal <= 0;
            --stable_counter <= 0;
            last_valor_max_janela <= (OTHERS => '0');
            data_out <= 0;
        ELSIF rising_edge(clk) AND read_enable = '1' THEN
            count_internal <= count_internal + 1;
            --last_valor_max_janela <= valor_max_janela;
            IF signed(valor_max_janela) > signed(last_valor_max_janela) THEN
                last_valor_max_janela <= valor_max_janela;
                data_out <= count_internal; -- position
            END IF;
            IF count_internal >= 54 THEN
                count_internal <= 0;
            END IF;

        END IF;
    END PROCESS;

END ARCHITECTURE Behavioral;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.comp_gen.ALL;

ENTITY ring_buffer_max_2 IS
    GENERIC (
        N : INTEGER := 18;
        Memo : INTEGER := 540 -- Alterado de 54 para 540
    );
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        data_out : OUT INTEGER RANGE 0 TO 539; -- Alterado de 55 para 539
        write_enable : IN STD_LOGIC;
        read_enable : IN STD_LOGIC;
        read_data : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END ENTITY ring_buffer_max_2;

ARCHITECTURE Behavioral OF ring_buffer_max_2 IS
    SIGNAL write_pointer : INTEGER := 0;
    SIGNAL read_pointer : INTEGER := 0;
    SIGNAL position : INTEGER;
    SIGNAL data_temp : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL valor_max_janela : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL max_temp : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL stable_counter : INTEGER := 0;
    SIGNAL last_valor_max_janela : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL count_internal : INTEGER := Memo; -- Alterado de 54 para Memo
    COMPONENT memory_01 IS
        GENERIC (
            N : INTEGER := 18;
            M : INTEGER := Memo -- Alterado de 54 para Memo
        );
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            data_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            write_pointer : IN INTEGER;
            read_pointer : IN INTEGER;
            write_enable : IN STD_LOGIC;
            read_enable : IN STD_LOGIC
        );
    END COMPONENT;
BEGIN

    mem_inst : memory_01
    PORT MAP(clk, reset, ecg_h, data_temp, write_pointer, read_pointer, write_enable, read_enable);
    max : magnitude_comparator GENERIC MAP(N) PORT MAP(clk, reset, data_temp, valor_max_janela);

    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            write_pointer <= 0;
            read_pointer <= 0;
        ELSIF rising_edge(clk) THEN
            IF write_enable = '1' THEN
                write_pointer <= (write_pointer + 1) MOD Memo; -- Alterado de 54 para Memo
            END IF;
            IF read_enable = '1' THEN
                read_pointer <= (read_pointer + 1) MOD Memo; -- Alterado de 54 para Memo
            END IF;
            IF write_pointer >= Memo THEN -- Alterado de 54 para Memo
                write_pointer <= 0;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (clk)
    BEGIN
        IF rising_edge(clk) THEN
            IF read_enable = '1' THEN
                read_data <= data_temp;
            END IF;
        END IF;
    END PROCESS;
    monitor_valor_max_janela : PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            max_temp <= "100000000000000000";
            --max_temp <= "000000000010011001";
            count_internal <= 0;
            last_valor_max_janela <= (OTHERS => '0');
            data_out <= 0;
        ELSIF rising_edge(clk) AND read_enable = '1' THEN
            count_internal <= count_internal + 1;
            IF signed(valor_max_janela) > signed(last_valor_max_janela) THEN
                last_valor_max_janela <= valor_max_janela;
                data_out <= count_internal;
            END IF;
            IF count_internal >= Memo THEN -- Alterado de 54 para Memo
                count_internal <= 0;
            END IF;
        END IF;
    END PROCESS;
END behavioral;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE comp_gen2 IS

    COMPONENT Meancalculator_2 IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            reset, Ld : IN STD_LOGIC;
            data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            mean_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT percent80 IS
        GENERIC (N : INTEGER := 18);
        PORT (
            --clk : in std_logic;
            --reset, Ld : in std_logic;
            data_in : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            data_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT sub_gen_25p IS
        GENERIC (N : INTEGER := 18);
        PORT (
            IN_1, IN_2 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            sub_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT sub_gen_125p IS -- Alterando o nome para refletir a nova constante
        GENERIC (N : INTEGER := 18);
        PORT (
            IN_1, IN_2 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            sub_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT sub_gen_125p_2 IS -- Alterando o nome para refletir a nova constante
        GENERIC (N : INTEGER := 18);
        PORT (
            IN_1, IN_2 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            sub_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT ring_buffer_max IS -- ring_buffer_max
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            data_out : OUT INTEGER RANGE 0 TO 55; -- Modificado de out para buffer
            write_enable : IN STD_LOGIC;
            read_enable : IN STD_LOGIC;
            read_data : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT ring_buffer_max_2 IS
        GENERIC (
            N : INTEGER := 18;
            Memo : INTEGER := 540 -- Alterado de 54 para 540
        );
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            data_out : OUT INTEGER RANGE 0 TO 539; -- Alterado de 55 para 539
            write_enable : IN STD_LOGIC;
            read_enable : IN STD_LOGIC;
            read_data : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

END comp_gen2;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_arith.ALL;
USE work.comp_gen.ALL;
USE work.comp_gen2.ALL;

ENTITY training_02 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        reset, Ld_treino : IN STD_LOGIC;
        ecg_m, ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1 : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
    );
END ENTITY training_02;

ARCHITECTURE Structural OF training_02 IS
    SIGNAL sum, out_soma : STD_LOGIC_VECTOR(2 * N - 1 DOWNTO 0);
    SIGNAL temp_THR_SIG : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL THR_SIG_signal, THR_NOISE_signal, THR_SIG1_signal, THR_NOISE1_signal : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL percent25_1 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL temp0, temp1, temp2, temp3, temp4 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL temp80, sum80_0 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    --constant factor_binary : STD_LOGIC_VECTOR (N-1 downto 0) := "000000000000000001";
    --constant percent80 : STD_LOGIC_VECTOR (N-1 downto 0) := "000000000110011001";
BEGIN
    --THR_SIG------------------------------------------------------------
    max_0 : max_out GENERIC MAP(N) PORT MAP(clk, reset, ecg_m, temp_THR_SIG);
    percent25_1 <= temp_THR_SIG(N - 1) & temp_THR_SIG(N - 1) & temp_THR_SIG(N - 1 DOWNTO 2);
    reg_0 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, percent25_1, THR_SIG_signal);
    THR_SIG <= THR_SIG_signal;
    ---------------------------------------------------------------------
    --SIG_LEV------------------------------------------------------------
    --SIG_LEV<=THR_SIG_signal;

    --NOISE_LEV----------------------------------------------------------------------
    mean_0 : Meancalculator_2 GENERIC MAP(N) PORT MAP(clk, reset, Ld_treino, ecg_m, temp0);
    percent80_0 : percent80 GENERIC MAP(N) PORT MAP(temp0, temp1);
    reg_1 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, temp1, NOISE_LEV);
    ------------------------------------------------------------------------------------ 
    --THR_NOISE--------------------------------------------------------------------
    mean_1 : Meancalculator_2 GENERIC MAP(N) PORT MAP(clk, reset, Ld_treino, ecg_m, temp2);
    THR_NOISE_signal <= temp2(N - 1) & temp2(N - 1) & temp2(N - 1 DOWNTO 2);
    THR_NOISE <= THR_NOISE_signal;
    -- THR_SIG1--------------------------------------------------------------------
    max_1 : magnitude_comparator GENERIC MAP(N) PORT MAP(clk, reset, ecg_h, temp3);
    THR_SIG1_signal <= temp3(N - 1) & temp3(N - 1) & temp3(N - 1 DOWNTO 2);
    reg_2 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, THR_SIG1_signal, THR_SIG1);
    --THR_SIG1<=THR_SIG1_signal;
    --THR_NOISE1--------------------------------------------------------------------
    mean_2 : Meancalculator_2 GENERIC MAP(N) PORT MAP(clk, reset, Ld_treino, ecg_h, temp4);
    THR_NOISE1_signal <= temp4(N - 1) & temp4(N - 1) & temp4(N - 1 DOWNTO 2);
    THR_NOISE1 <= THR_NOISE1_signal;
    -------------------------------------------------------------------------
    --SIG_LEV1<=THR_SIG1_signal;
    --NOISE_LEV1<=THR_NOISE1_signal;

END ARCHITECTURE Structural;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
--USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;
USE work.comp_gen.ALL;
USE work.comp_gen2.ALL;

ENTITY conta_beat_6 IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        load : IN STD_LOGIC;
        ecg_m, ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_THR_SIG, out_SIG_LEV, out_NOISE_LEV : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0); --out_THR_NOISE
        out_THR_SIG1, out_SIG_LEV1, out_NOISE_LEV1, out_THR_NOISE1 : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_valor_arm, out_valor_arm_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_position_h : OUT INTEGER RANGE 0 TO 4000;
        --out_put : out std_logic_vector(N-1 downto 0)
        r_peak_number : OUT INTEGER RANGE 0 TO 4000
        --count : out integer range 0 to 1000
    );
END conta_beat_6;

ARCHITECTURE bh OF conta_beat_6 IS
    SIGNAL position_h, i_bat_saida, temp_position_h, teste_position, temp_potential_peak_index : INTEGER;
    SIGNAL Tive : STD_LOGIC;
    SIGNAL count_internal : INTEGER := 513; -- valor inicial
    SIGNAL SIG_LEV_TEMP, NOISE_LEV_TEMP, THR_SIG_TEMP, THR_SIG1_TEMP, SIG_LEV1_TEMP, NOISE_LEV1_TEMP, out_125_1, out_125_2 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL pulso_sig_lev, control_sig_lev, pulso_sig_lev1, control_sig_lev1 : STD_LOGIC;
    SIGNAL pulso_thr_sig, control_thr_sig : STD_LOGIC;
    SIGNAL pulso_noise_lev, control_noise_lev : STD_LOGIC;
    SIGNAL pulso_thr_sig1, control_thr_sig1 : STD_LOGIC;
    SIGNAL valor_arm, TEMP_val_arm, val_arm_recorded, valor_arm_h, amp_bat_saida, val_arm_flag, potential_peak_value : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    --SIGNAL NOISE_LEV_PIP_IN, NOISE_LEV1_PIP_IN, SIG_LEV_PIP_IN, SIG_LEV1_PIP_IN, THR_NOISE_PIP_IN, THR_NOISE1_PIP_IN, THR_SIG_PIP_IN, THR_SIG1_PIP_IN : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    --SIGNAL NOISE_LEV_IN, NOISE_LEV1_IN, SIG_LEV_IN, SIG_LEV1_IN, THR_NOISE_IN, THR_NOISE1_IN, THR_SIG_IN, THR_SIG1_IN : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12, temp13 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL In_thr_sig, In_thr_sig1 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL entrada_ld_noise : STD_LOGIC;
    SIGNAL OUTMUX_SIG_LEV, OUTMUX_SIG_LEV1, out_calc_SIG_LEV, out_calc_SIG_LEV1, out_calc_NOISE_LEV, out_ring_buffer, out_ring_buffer2 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    --signal out_memo1 std_logic_vector(N-1 downto 0);
    SIGNAL Ld_arm_h, Ld_sig_lev_temp, Ld_sig_lev_temp_1, Ld_sig_lev1_temp, Ld_sig_lev1_temp_1, Ld_noise_lev_temp, Ld_noise_lev_temp_1, Ld_thr_sig_temp, Ld_thr_sig_temp_1, Ld_thr_sig1_temp, Ld_thr_sig1_temp_1, Ld_noise1_lev, Ld_sig_lev1_out : STD_LOGIC;
    SIGNAL ld_val_arm, reset_val_arm : STD_LOGIC;
    SIGNAL ctrl_read_1, ctrl_write_1, SEL_INIT, SEL_INIT_thr, sel_reg_slt, sel_reg_noise, reset_janela : STD_LOGIC;
    SIGNAL SEL_ENTRADA, OPERA_SIG_LEV, OPERA_SIG_LEV1, OPERA_NOISE_LEV, flag_WI : STD_LOGIC;
    SIGNAL AUX00, AUX0, AUX1, AUX2, AUX3 : STD_LOGIC;
    SIGNAL w_pointer_1, r_pointer1 : INTEGER := 0;
    SIGNAL write_enable, read_enable, write_enable_perd, read_enable_perd : STD_LOGIC;
    CONSTANT active_reg : STD_LOGIC_VECTOR := "000000010000000000";
    --constant numero_amostras_max_RR:   :="1000011100"; --540
    SIGNAL a : INTEGER := 2;
    SIGNAL delay : INTEGER := 54;
    CONSTANT X : INTEGER := 10; -- Defina o valor de X conforme necessário
    SIGNAL trigger_signal : STD_LOGIC := '0'; -- Sinal de ativação

    SIGNAL count_internal_2, last_beat_sample, current_beat_time, RR_interval, VAR_DIFF, potencial_peak_index : INTEGER := 0;
    CONSTANT COUNT_LIMIT : INTEGER := 54; -- 
    CONSTANT COUNT_LIMIT_2 : INTEGER := 540; -- COUNT_LIMIT_2
    CONSTANT numero_amostras_max_RR : INTEGER := 540; -- numero_amostras_max_RR
    CONSTANT numero_amostras_min_RR : INTEGER := 108;
    CONSTANT fs : INTEGER := 360;
    SIGNAL flag_count, flag_update_val, LD_VAL_ARM2, reset_val_arm2, reset_val_arm_h, reset_max_finder, aux_sig_lev, reset_perdido, reset_max_perdido, flag_perdido, flat_lost : STD_LOGIC;
    SIGNAL VAL_ARM_2 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL fast_clk, clk_teste : STD_LOGIC := '0'; -- Clock rápido
    CONSTANT clk_period : TIME := 100 ns; -- Período do clk principal
    CONSTANT fast_clk_period : TIME := 50 ns; -- Período do fast_clk (mais rápido)
    SIGNAL flag_executed : STD_LOGIC := '0';
    SIGNAL reset_control : STD_LOGIC := '0'; -- Variável de controle para reset
    SIGNAL previous_Tive : STD_LOGIC := '0'; -- Variável para armazenar o valor anterior de Tive
    SIGNAL cycle_count : INTEGER := 0;
    SIGNAL cycle_count_x : INTEGER := 0;
    SIGNAL counting : BOOLEAN := FALSE; -- variável para armazenar o estado de contagem

BEGIN
    ---------------------ecg_h----------------------------------
    --valor_arm_h = max(ecg_h); pega 
    max_ecgh : magnitude_comparator GENERIC MAP(N) PORT MAP(clk, reset_val_arm_h, ecg_h, valor_arm_h); -- tem reg interno
    -- SIG_LEV1 = 0.125*valor_arm_h + 0.875*SIG_LEV1 ; 
    -- SIG_LEV1 = 0.125*valor_arm_h + 0.875*SIG_LEV1 ; 
    mux_sig_lev1 : MUX2_1_GEN GENERIC MAP(N) PORT MAP(SIG_LEV1, SIG_LEV1_TEMP, SEL_INIT, OUTMUX_SIG_LEV1);
    reg_SIG_LEV1_TEMP : REG_GEN GENERIC MAP(N) PORT MAP(clk, AUX00, reset, OUTMUX_SIG_LEV1, temp13);
    --mux_bit_00 : MUX2_1_GEN_bit PORT MAP(Ld_sig_lev1_temp, Ld_sig_lev1_temp_1, SEL_INIT, AUX00);
    mux_bit_00 : MUX2_1_GEN_bit PORT MAP(Ld_sig_lev1_temp, control_sig_lev1, SEL_INIT, AUX00);

    calc_SIG_LEV1 : calc GENERIC MAP(N) PORT MAP(valor_arm_h, temp13, out_calc_SIG_LEV1);
    --mux_reg_or_cal_sig_lev1: MUX2_1_GEN GENERIC MAP(N) PORT MAP(temp13, out_calc_SIG_LEV1, OPERA_SIG_LEV1, SIG_LEV1_TEMP);
    mux_reg_or_cal_sig_lev1 : MUX2_1_GEN GENERIC MAP(N) PORT MAP(temp13, out_calc_SIG_LEV1, control_sig_lev1, SIG_LEV1_TEMP);
    --reg_SIG_LEV1_TEMP : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_sig_lev1_out, reset, temp4, SIG_LEV1_TEMP);
    pulse_0 : PulseGenerator PORT MAP(clk, reset, pulso_sig_lev1, control_sig_lev1);

    ---------------------ecg_m----------------------------------

    -- SIG_LEV = 0.125*valor_arm + 0.875*SIG_LEV;
    mux_sig_lev : MUX2_1_GEN GENERIC MAP(N) PORT MAP(SIG_LEV, SIG_LEV_TEMP, SEL_INIT, OUTMUX_SIG_LEV);
    reg_SIG_LEV_TEMP : REG_GEN GENERIC MAP(N) PORT MAP(clk, AUX0, reset, OUTMUX_SIG_LEV, temp3);
    --mux_bit_0 : MUX2_1_GEN_bit PORT MAP(Ld_sig_lev_temp, Ld_sig_lev_temp_1, SEL_INIT, AUX0);
    mux_bit_0 : MUX2_1_GEN_bit PORT MAP(Ld_sig_lev_temp, control_sig_lev, SEL_INIT, AUX0);
    --calc_SIG_LEV : calc GENERIC MAP(N) PORT MAP(val_arm_recorded, temp3, out_calc_SIG_LEV);
    --calc_SIG_LEV : calc_m GENERIC MAP(N) PORT MAP(val_arm_recorded, temp3, out_calc_SIG_LEV);
    calc_SIG_LEV : calc GENERIC MAP(N) PORT MAP(val_arm_recorded, temp3, out_calc_SIG_LEV);
    --calc_SIG_LEV : calc GENERIC MAP(N) PORT MAP(valor_arm, temp3, out_calc_SIG_LEV);
    --calc_SIG_LEV : calc GENERIC MAP(N) PORT MAP(valor_arm, temp3, out_calc_SIG_LEV);
    --mux_reg_or_cal_sig_lev: MUX2_1_GEN GENERIC MAP(N) PORT MAP(temp3, out_calc_SIG_LEV, OPERA_SIG_LEV, SIG_LEV_TEMP);
    mux_reg_or_cal_sig_lev : MUX2_1_GEN GENERIC MAP(N) PORT MAP(temp3, out_calc_SIG_LEV, control_sig_lev, SIG_LEV_TEMP);
    pulse_1 : PulseGenerator PORT MAP(clk, reset, pulso_sig_lev, control_sig_lev); --aux_sig_lev
    --pulse_1 : PulseGenerator PORT MAP(clk, reset, aux_sig_lev, control_sig_lev); --aux_sig_lev

    -- NOISE_LEV = 0.125*valor_arm + 0.875*NOISE_LEV; % reajuste NOISE_LEV
    mux_noise : MUX2_1_GEN GENERIC MAP(N) PORT MAP(NOISE_LEV, NOISE_LEV_TEMP, SEL_INIT, temp12);
    reg_NOISE_LEV_TEMP : REG_GEN GENERIC MAP(N) PORT MAP(clk, AUX1, reset, temp12, temp5);
    --mux_bit_1 : MUX2_1_GEN_bit PORT MAP(Ld_noise_lev_temp, Ld_noise_lev_temp_1, SEL_INIT, AUX1);
    mux_bit_1 : MUX2_1_GEN_bit PORT MAP(Ld_noise_lev_temp, control_noise_lev, SEL_INIT, AUX1);
    --calc_NOISE_LEV : calc GENERIC MAP(N) PORT MAP(val_arm_recorded, temp5, out_calc_NOISE_LEV);
    --calc_NOISE_LEV : calc GENERIC MAP(N) PORT MAP(valor_arm, temp5, out_calc_NOISE_LEV);
    calc_NOISE_LEV : calc_3 GENERIC MAP(N) PORT MAP(valor_arm, temp5, out_calc_NOISE_LEV);
    mux_reg_or_cal_noise_lev : MUX2_1_GEN GENERIC MAP(N) PORT MAP(temp5, out_calc_NOISE_LEV, control_noise_lev, NOISE_LEV_TEMP);
    pulse_noise : PulseGenerator PORT MAP(clk, reset, pulso_noise_lev, control_noise_lev);

    -- THR_SIG = NOISE_LEV + 0.25*(abs(SIG_LEV - NOISE_LEV));
    --mux_thr_sig : MUX2_1_GEN GENERIC MAP(N) PORT MAP(THR_SIG, temp7, SEL_INIT, temp8);
    --mux_thr_sig : MUX2_1_GEN GENERIC MAP(N) PORT MAP(THR_SIG, temp7, SEL_INIT_thr, temp8);
    mux_thr_sig : MUX2_1_GEN GENERIC MAP(N) PORT MAP(In_thr_sig, temp7, SEL_INIT_thr, temp8);
    --sub_25p_0 : sub_gen_25p GENERIC MAP(N) PORT MAP(SIG_LEV_TEMP, NOISE_LEV_TEMP, clk, reset, temp6);
    sub_25p_0 : sub_gen_125p GENERIC MAP(N) PORT MAP(SIG_LEV_TEMP, NOISE_LEV_TEMP, clk, reset, temp6);
    --temp6 <= out_125_1(N-1) & out_125_1(N-2 DOWNTO 0);
    --sub_25p_0 : sub_gen_125p_2 GENERIC MAP(N) PORT MAP(SIG_LEV_TEMP, NOISE_LEV_TEMP, clk, reset, temp6);
    sum_0 : SUM_GEN_2 GENERIC MAP(N) PORT MAP(NOISE_LEV_TEMP, temp6, temp7);
    reg_thr_sig : REG_GEN GENERIC MAP(N) PORT MAP(clk, AUX2, reset, temp8, THR_SIG_TEMP); -- usado primeira vez
    mux_thr_in : MUX2_1_GEN GENERIC MAP(N) PORT MAP(THR_SIG, THR_SIG_TEMP, SEL_INIT_thr, In_thr_sig);
    --mux_bit_3 : MUX2_1_GEN_bit PORT MAP(Ld_thr_sig_temp, Ld_thr_sig_temp_1, SEL_INIT, AUX2);
    --mux_bit_3 : MUX2_1_GEN_bit PORT MAP(Ld_thr_sig_temp, control_thr_sig, SEL_INIT, AUX2);
    mux_bit_3 : MUX2_1_GEN_bit PORT MAP(Ld_thr_sig_temp, control_thr_sig, SEL_INIT_thr, AUX2);
    pulse_2 : PulseGenerator PORT MAP(clk, reset, pulso_thr_sig, control_thr_sig);

    -- THR_SIG1 = NOISE_LEV1 + 0.25*(abs(SIG_LEV1 - NOISE_LEV1));
    --mux_thr_sig1 : MUX2_1_GEN GENERIC MAP(N) PORT MAP(THR_SIG1, temp10, SEL_INIT, temp11);
    mux_thr_sig1 : MUX2_1_GEN GENERIC MAP(N) PORT MAP(THR_SIG1, temp10, SEL_INIT_thr, temp11);
    --sub_25p_1 : sub_gen_25p GENERIC MAP(N) PORT MAP(SIG_LEV1_TEMP, NOISE_LEV1_TEMP, clk, reset, temp9);
    sub_25p_1 : sub_gen_125p GENERIC MAP(N) PORT MAP(SIG_LEV1_TEMP, NOISE_LEV1_TEMP, clk, reset, temp9);
    --temp9 <= out_125_1(N-1) & out_125_1(N-2 DOWNTO 0);
    --sub_25p_1 : sub_gen_125p_2 GENERIC MAP(N) PORT MAP(SIG_LEV1_TEMP, NOISE_LEV1_TEMP, clk, reset, temp9);
    sum_1 : SUM_GEN_2 GENERIC MAP(N) PORT MAP(NOISE_LEV_TEMP, temp9, temp10);
    reg_thr_sig1 : REG_GEN GENERIC MAP(N) PORT MAP(clk, AUX3, reset, temp11, THR_SIG1_TEMP); -- usado primeira vez
    mux_thr_in1 : MUX2_1_GEN GENERIC MAP(N) PORT MAP(THR_SIG, THR_SIG1_TEMP, SEL_INIT_thr, In_thr_sig1);
    --mux_bit_4 : MUX2_1_GEN_bit PORT MAP(Ld_thr_sig1_temp, Ld_thr_sig1_temp_1, SEL_INIT, AUX3);
    --mux_bit_4 : MUX2_1_GEN_bit PORT MAP(Ld_thr_sig1_temp, control_thr_sig1, SEL_INIT, AUX3);
    mux_bit_4 : MUX2_1_GEN_bit PORT MAP(Ld_thr_sig1_temp, control_thr_sig1, SEL_INIT_thr, AUX3);
    pulse_4 : PulseGenerator PORT MAP(clk, reset, pulso_thr_sig1, control_thr_sig1);

    -- NOISE_LEV1 = THR_NOISE1;
    reg_noise_lev1 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_noise1_lev, reset, THR_NOISE1, NOISE_LEV1_TEMP); -- usado primeira vez
    reg_val_arm : REG_GEN GENERIC MAP(N) PORT MAP(clk, ld_val_arm, reset_val_arm, valor_arm, val_arm_recorded);

    -- position janela de 54
    ring_buffer_1 : ring_buffer_max GENERIC MAP(N) PORT MAP(clk, reset_janela, ecg_h, temp_position_h, write_enable, read_enable, out_ring_buffer);
    --max_finder_flag : MaxFinder_2 GENERIC MAP(N) PORT MAP(clk, reset, ecg_m, flag_WI, val_arm_flag);
    --max_finder_flag : MaxFinder_2 GENERIC MAP(N) PORT MAP(clk, reset_max_finder, ecg_m, flag_WI, val_arm_flag);
    max_finder_flag : MaxFinder_3 GENERIC MAP(N) PORT MAP(clk, reset_max_finder, ecg_m, flag_WI, val_arm_flag);
    --REG_VAL_AMR:
    REG_VAL_ARM2 : REG_GEN GENERIC MAP(N) PORT MAP(clk, LD_VAL_ARM2, reset_val_arm2, valor_arm, VAL_ARM_2);

    --teste de atraso
    --atraso_val_arm: edge_detector GENERIC MAP(8) PORT MAP(clk, reset, pulso_sig_lev, aux_sig_lev);

    -- reset val_arm
    reset_arm : edge_detector GENERIC MAP(20) PORT MAP(clk, reset, pulso_sig_lev, reset_val_arm);

    --reset max finder
    --reset_maxfinder : edge_detector GENERIC MAP(25) PORT MAP(clk, reset, flag_WI, reset_max_finder);
    --reset_maxfinder : edge_detector GENERIC MAP(75) PORT MAP(clk, reset, flag_WI, reset_max_finder);
    reset_maxfinder : edge_detector_2 GENERIC MAP(25, 5) PORT MAP(clk, reset, flag_WI, reset_max_finder);
    --reset_maxfinder : edge_detector_3 GENERIC MAP(25, 5) PORT MAP(clk, reset, flag_WI, reset_max_finder);

    -- reset val_arm_h
    reset_arm_h : edge_detector GENERIC MAP(10) PORT MAP(clk, reset, pulso_sig_lev1, reset_val_arm_h);

    -- busca perdido
    ring_buffer_2 : ring_buffer_max_2 GENERIC MAP(N, COUNT_LIMIT_2) PORT MAP(clk, reset_perdido, ecg_m, temp_potential_peak_index, write_enable_perd, read_enable_perd, out_ring_buffer2);
    max_finder_flag_perdido : MaxFinder_3 GENERIC MAP(N) PORT MAP(clk, reset_max_perdido, out_ring_buffer2, flag_perdido, potential_peak_value);
    reset_maxfinder_perdido : edge_detector_2 GENERIC MAP(25, 5) PORT MAP(clk, reset, flag_perdido, reset_max_perdido);

    PROCESS (clk, reset)
        --VARIABLE reset_control : STD_LOGIC := '0'; -- Variável de controle para reset
        --VARIABLE previous_Tive : STD_LOGIC := '0'; -- Variável para armazenar o valor anterior de Tive

    BEGIN
        IF reset = '1' THEN
            reset_control <= '0'; -- Limpa a variável de controle no reset
            valor_arm <= (OTHERS => '0');
            amp_bat_saida <= (OTHERS => '0');
            Tive <= '0';
            SEL_INIT <= '0';
            -- sinal do sig1 vem pelo bloco max que tem um reg
            Ld_sig_lev1_temp <= '1';
            Ld_sig_lev_temp <= '1'; -- pega sinal de fora e armazena 
            Ld_sig_lev_temp_1 <= '0'; -- faz a operaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o 
            Ld_sig_lev1_temp_1 <= '0';
            Ld_noise_lev_temp <= '1'; -- pega sinal de fora 
            Ld_noise_lev_temp_1 <= '0'; -- faz operaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o 
            Ld_thr_sig_temp <= '1'; -- pega sinal de fora
            Ld_thr_sig_temp_1 <= '0'; -- faz operaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o 
            Ld_thr_sig1_temp <= '1'; -- pega sinal de fora
            Ld_thr_sig1_temp_1 <= '0'; -- faz operaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o 
            Ld_noise1_lev <= '1'; -- pega sinal de fora

            i_bat_saida <= 0;
            count_internal <= 0;
            last_beat_sample <= 0;
            SEL_INIT <= '0';
            SEL_INIT_thr <= '0';
            sel_reg_slt <= '0';
            entrada_ld_noise <= '1';
            sel_reg_noise <= '0';
            OPERA_SIG_LEV1 <= '0';
            OPERA_SIG_LEV <= '0';
            OPERA_NOISE_LEV <= '0';
            pulso_sig_lev <= '0';
            pulso_thr_sig <= '0';
            pulso_noise_lev <= '0';
            pulso_sig_lev1 <= '0';
            --reset_janela <= '1';
            teste_position <= 38;
            flag_update_val <= '0';
            TEMP_val_arm <= (OTHERS => '0');
            --sel_noise_in <= '0';
            --LD_VAL_ARM2 <= '0';
            reset_val_arm2 <= '1';
            --reset_val_arm_h <= '1';
            --reset_val_arm <= '1';
            ld_val_arm <= '0';
            flat_lost <= '0';

        ELSIF rising_edge(clk) AND load = '1' THEN
            SEL_INIT <= '1';
            -- sinal do sig1 vem pelo bloco max que tem um reg
            Ld_sig_lev1_temp <= '0';
            Ld_sig_lev_temp <= '0'; -- pega sinal de fora e armazena 
            Ld_noise_lev_temp <= '0'; -- pega sinal de fora 
            Ld_thr_sig_temp <= '0'; -- pega sinal de fora
            Ld_thr_sig1_temp <= '0'; -- pega sinal de fora
            Ld_noise1_lev <= '0'; -- pega sinal de fora
            count_internal <= count_internal + 1; -- incrementa a contagem
            reset_val_arm2 <= '0';
            IF reset_control = '1' THEN -- Verifica a variável de controle na borda de subida
                reset_control <= '0'; -- Limpa a variável de controle

            END IF;
            IF SIGNED(ecg_m) > SIGNED(NOISE_LEV_TEMP) THEN -- if ecg_m5(i)> NOISE_LEV

                IF SIGNED(valor_arm) <= SIGNED(ecg_m) THEN
                    valor_arm <= ecg_m;
                    ld_val_arm <= '1';
                    Tive <= '1';

                ELSE
                    ld_val_arm <= '0';

                END IF;

            ELSIF Tive = '1' THEN -- if Tive
                flag_executed <= '0'; -- Reseta a flag quando TIVE = 1

                previous_Tive <= Tive; -- Atualiza a variável previous_Tive para o próximo ciclo de clock

                IF signed(val_arm_recorded) >= signed(THR_SIG_TEMP) THEN -- if valor_arm >= THR_SIG
                    pulso_sig_lev <= '1'; -- SIG_LEV = 0.125*valor_arm + 0.875*SIG_LEV;
                    --IF position_h - i_bat_saida >= 75 AND valor_arm_h >= THR_SIG1_TEMP THEN --if position_h - i_bat_saida(a-1)>=75 && valor_arm_h >= THR_SIG1
                    IF signed(valor_arm_h) >= signed(THR_SIG1_TEMP) THEN

                        IF position_h = 0 THEN
                            pulso_sig_lev1 <= '1';--SIG_LEV1 = 0.125*valor_arm_h + 0.875*SIG_LEV1;
                            amp_bat_saida <= valor_arm_h;
                            i_bat_saida <= position_h;
                            a <= a + 1;
                            Tive <= '0';
                            reset_val_arm2 <= '1'; --valor_arm=0;
                            last_beat_sample <= count_internal - 38;

                        ELSE
                            --reset_val_arm <= '0';

                        END IF;
                        IF position_h - i_bat_saida >= 75 AND valor_arm_h >= THR_SIG1_TEMP THEN --if position_h - i_bat_saida(a-1)>=75 && valor_arm_h >= THR_SIG1
                            --IF position_h - i_bat_saida >= 75 THEN --if position_h - i_bat_saida(a-1)>=75 && valor_arm_h >= THR_SIG1
                            flag_update_val <= '1';
                            amp_bat_saida <= valor_arm_h;
                            i_bat_saida <= position_h;
                            pulso_sig_lev1 <= '1';--SIG_LEV1 = 0.125*valor_arm_h + 0.875*SIG_LEV1;
                            a <= a + 1;
                            Tive <= '0';
                            reset_val_arm2 <= '1'; --valor_arm=0;
                            last_beat_sample <= count_internal - 38;

                        END IF;

                        IF i_bat_saida > 0 THEN -- Evita calcular a frequência cardíaca no primeiro batimento
                            RR_interval <= position_h - i_bat_saida; -- Intervalo RR em amostras
                            --current_heart_rate = 60 / RR_interval;  -- Frequência cardíaca atual em bpm
                        END IF;

                        IF a > 2 THEN -- 
                            SEL_INIT_thr <= '0';

                        END IF;
                        --IF previous_Tive = '1' AND Tive = '0' THEN
                        --END IF;
                    END IF;

                    --ELSIF VAR_DIFF >= numero_amostras_max_RR THEN
                ELSIF (count_internal - last_beat_sample) >= numero_amostras_max_RR THEN

                    IF flag_executed = '0' THEN
                        Tive <= '0';
                        flat_lost <= '1';
                        a <= a + 1;
                        amp_bat_saida <= potential_peak_value;
                        flag_executed <= '1'; -- Seta a flag indicando que o código foi executado
                    END IF;
                    --END IF;
                ELSE
                    flat_lost <= '0';
                END IF;

                --END IF;

            ELSIF Tive = '0' AND previous_Tive = '1' AND load = '1' THEN
                valor_arm <= (OTHERS => '0'); -- Reset valor_arm
                trigger_signal <= '1'; -- Ativa o sinal de ativação na borda de descida de Tive
                pulso_thr_sig <= '1'; --  THR_SIG = NOISE_LEV + 0.25*(abs(SIG_LEV - NOISE_LEV)); 
                pulso_noise_lev <= '1'; -- NOISE_LEV = 0.125*valor_arm + 0.875*NOISE_LEV; % reajuste NOISE_LEV
                pulso_sig_lev1 <= '1'; -- SIG_LEV1 = 0.125*valor_arm_h + 0.875*SIG_LEV1;
                pulso_thr_sig1 <= '1'; -- THR_SIG1 = NOISE_LEV1 + 0.25*(abs(SIG_LEV1 - NOISE_LEV1));
                --reset_val_arm2 <='1';
                --reset_val_arm_h <= '1';
                previous_Tive <= '0';
            ELSE
                trigger_signal <= '0'; -- Desativa o sinal de ativação em todos os outros casos
                pulso_thr_sig <= '0';
                pulso_noise_lev <= '0';
                pulso_sig_lev <= '0';
                pulso_sig_lev1 <= '0';
                pulso_thr_sig1 <= '0';
                --reset_val_arm2 <='0';
                --reset_val_arm_h <='0';
            END IF;

        END IF;
        --END IF;
    END PROCESS;
    r_peak_number <= a;
    out_SIG_LEV <= SIG_LEV_TEMP;
    out_SIG_LEV1 <= SIG_LEV1_TEMP;
    --out_SIG_LEV1 <= temp4;
    out_THR_SIG <= THR_SIG_TEMP;
    out_THR_SIG1 <= THR_SIG1_TEMP;
    out_NOISE_LEV <= NOISE_LEV_TEMP;
    out_NOISE_LEV1 <= NOISE_LEV1_TEMP;
    out_THR_NOISE1 <= NOISE_LEV1_TEMP;
    out_valor_arm <= valor_arm;
    out_valor_arm_h <= valor_arm_h;
    out_position_h <= delay;
    PROCESS (clk, reset) --  SIG_LEV = 0.125*valor_arm + 0.875*SIG_LEV;
        --VARIABLE cycle_count : INTEGER := 0;
    BEGIN
        IF reset = '1' THEN
            --val_arm <= (OTHERS => '0');
            cycle_count <= 0;
            --reset_val_arm <= '1';
            --ld_val_arm <= '0';
        ELSIF rising_edge(clk) AND load = '1' THEN
            --reset_val_arm <= '0';
            IF pulso_sig_lev <= '1' THEN
                --ld_val_arm <= '1';

            END IF;

            -- Incrementa a contagem de ciclos
            cycle_count <= cycle_count + 1;

            -- Reset val_arm após X ciclos de clock
            IF cycle_count >= X THEN
                --val_arm <= (OTHERS => '0');
                --ld_val_arm <= '0';
                --reset_val_arm <= '0';
                cycle_count <= 0; -- Reset a contagem de ciclos
            END IF;
        END IF;
    END PROCESS;

    PROCESS (clk, reset) --  SIG_LEV1 = 0.125*valor_arm_h + 0.875*SIG_LEV1 ;
        --VARIABLE cycle_count_x : INTEGER := 0;
    BEGIN
        IF reset = '1' THEN
            --val_arm <= (OTHERS => '0');
            cycle_count_x <= 0;
            --reset_val_arm <= '1';
            --reset_val_arm_h <= '1';
            --ld_val_arm <= '0';
        ELSIF rising_edge(clk) AND load = '1' THEN
            --reset_val_arm <= '0';
            --reset_val_arm_h <= '0';
            IF pulso_sig_lev1 <= '1' THEN
                --ld_val_arm <= '1';

            END IF;

            -- Incrementa a contagem de ciclos
            cycle_count_x <= cycle_count_x + 1;

            -- Reset val_arm após X ciclos de clock
            IF cycle_count_x >= X THEN
                --val_arm <= (OTHERS => '0');
                --ld_val_arm <= '0';
                --reset_val_arm_h <= '0';
                cycle_count_x <= 0; -- Reset a contagem de ciclos
            END IF;
        END IF;
    END PROCESS;

    PROCESS (clk, reset) -- controle da janela 
        --VARIABLE counting : BOOLEAN := FALSE; -- variável para armazenar o estado de contagem
    BEGIN
        IF reset = '1' THEN
            count_internal_2 <= 0;
            write_enable <= '1';
            read_enable <= '0';
            reset_janela <= '1';
            counting <= FALSE; -- resetar o estado de contagem
        ELSIF rising_edge(clk) AND load = '1' THEN
            reset_janela <= '0';
            IF flag_WI = '1' THEN
                counting <= TRUE; -- iniciar a contagem quando flag_WI é ativado
                write_enable <= '0';
                read_enable <= '1';

            END IF;

            IF counting = TRUE THEN
                count_internal_2 <= count_internal_2 + 1; -- incrementar o contador enquanto estiver no estado de contagem
            END IF;

            IF count_internal_2 >= COUNT_LIMIT THEN
                count_internal_2 <= 0; -- resetar o contador
                counting <= FALSE; -- resetar o estado de contagem
                write_enable <= '1';
                read_enable <= '0';
                reset_janela <= '1';
                flag_count <= '1';
            ELSE
                flag_count <= '0';
                --write_enable <= '0';
                --read_enable <= '1';
            END IF;
        END IF;
    END PROCESS;

    PROCESS (clk) -- 
    BEGIN

        IF reset = '1' THEN
            position_h <= 0; -- reset para valor inicial
        ELSIF rising_edge(clk) THEN -- 
            IF flag_count = '1' THEN -- Verifica se flag_count está ativo
                position_h <= temp_position_h + count_internal - delay - 39; -- Executa a operação desejada
            END IF;
        END IF;
    END PROCESS;
    PROCESS (clk) -- 
    BEGIN

        IF reset = '1' THEN
            --position_h <= 0; -- reset para valor inicial
        ELSIF rising_edge(clk) AND load = '1' THEN -- 
            IF Tive = '1' THEN -- Verifica se flag_count está ativo
                -- Executa a operação desejada
            END IF;
        END IF;
    END PROCESS;

    --PROCESS (fast_clk, reset)
    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            -- Reseta o estado inicial
            write_enable_perd <= '0';
            read_enable_perd <= '0';
            --ELSIF rising_edge(fast_clk) THEN
        ELSIF rising_edge(clk) AND load = '1' THEN
            IF flat_lost = '1' THEN
                -- Quando flat_lost é '1', inicia a leitura no clock mais rápido
                read_enable_perd <= '1';
                write_enable_perd <= '0';
                IF flag_perdido = '1' THEN
                    potencial_peak_index <= temp_potential_peak_index + last_beat_sample;
                END IF;
            ELSE
                -- Em estado normal, apenas escreve no buffer
                read_enable_perd <= '0';
                write_enable_perd <= '1';
            END IF;
        END IF;
    END PROCESS;
END bh;
--END Behavioral;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_signed.ALL;
USE work.comp_gen.ALL;
--USE work.comp_somadores.all;
-----------------------------------------------------
ENTITY pass IS
    GENERIC (
        N : INTEGER := 18
        --K : INTEGER := 16
        );
    PORT (
        clear, clk, ld_l : IN STD_LOGIC;
        X : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
        --A1, A2, A3, A4: IN STD_LOGIC_VECTOR(17 DOWNTO 0);
        S, S_der : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0));
END pass;

-----------------------------------------------------
ARCHITECTURE comportamento OF pass IS
    SIGNAL X_inv, X_inv2, X_inv4 : STD_LOGIC_VECTOR(13 DOWNTO 0);
    SIGNAL X0, X1, X2, X3, X4 : STD_LOGIC_VECTOR(12 DOWNTO 0);
    SIGNAL Y0, Y1, Y2, Y3, Y4 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL Sum01, Sum02, T0 : STD_LOGIC_VECTOR(13 DOWNTO 0);
    SIGNAL M0_trunc, Sum03, Sum04, Sum05, Sum06, Sum07, Sum08, Sum09, T1, T2, T3, M0_tr, M1_tr, M2_tr, M3_tr : STD_LOGIC_VECTOR(17 DOWNTO 0);
    SIGNAL Mult0, Mult1, Mult2, Mult3 : STD_LOGIC_VECTOR(35 DOWNTO 0);
    SIGNAL X0_1_2, X1_1_4, X3_1_2, X4_1_4 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL A0_aux0, A0_aux6, A0_aux8, A0_aux9, A0_aux11, A0_aux14, A0_aux15 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL SumA00, SumA01, SumA02, SumA03, SumA04, SumA05 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL A1_aux0, A1_aux3, A1_aux6, A1_aux10, A1_aux11, A1_aux16, A1_aux17 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL SumA10, SumA11, SumA12, SumA13, SumA14, SumA15, TA1_aux17 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL A2_aux0, A2_aux1, A2_aux3, A2_aux4, A2_aux6, A2_aux7, A2_aux10, A2_aux15 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL SumA20, SumA21, SumA22, SumA23, SumA24, SumA25, SumA26 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL A3_aux0, A3_aux3, A3_aux5, A3_aux6, A3_aux10, A3_aux11, A3_aux12 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL SumA30, SumA31, SumA32, SumA33, SumA34, SumA35 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL SumA05_aux, SumA15_aux, SumA26_aux, SumA35_aux : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
BEGIN
    R0_l : REG_GEN GENERIC MAP(13) PORT MAP(clk, ld_l, clear, X, X0); -- 0
    R1_l : REG_GEN GENERIC MAP(13) PORT MAP(clk, ld_l, clear, X0, X1); -- 1
    R2_l : REG_GEN GENERIC MAP(13) PORT MAP(clk, ld_l, clear, X1, X2); -- 2
    R3_l : REG_GEN GENERIC MAP(13) PORT MAP(clk, ld_l, clear, X2, X3); -- 3
    R4_l : REG_GEN GENERIC MAP(13) PORT MAP(clk, ld_l, clear, X3, X4); -- 4

    R5_l : REG_GEN GENERIC MAP(N) PORT MAP(clk, ld_l, clear, Sum06, Y0); -- 0
    R6_l : REG_GEN GENERIC MAP(N) PORT MAP(clk, ld_l, clear, Y0, Y1); -- 1 
    R7_l : REG_GEN GENERIC MAP(N) PORT MAP(clk, ld_l, clear, Y1, Y2); -- 2  
    R8_l : REG_GEN GENERIC MAP(N) PORT MAP(clk, ld_l, clear, Y2, Y3); -- 3  
    R9_l : REG_GEN GENERIC MAP(N) PORT MAP(clk, ld_l, clear, Y3, Y4); -- 4

    X_inv <= X0(12) & X0(12) & X0(11 DOWNTO 0);
    X_inv2 <= X2(12) & X2(11 DOWNTO 0) & '0';
    X_inv4 <= X4(12) & X4(12) & X4(11 DOWNTO 0);
    --S= B0x[n]-B2 x[n-2] +B4 x[n-4] +A1 y[n-1] -A2 y[n-2] +A3 y[n-3] -A4 y[n-4] 
    Tt0 : twoscompliment GENERIC MAP(14) PORT MAP(X_inv2, T0);
    SM0 : SUM_GEN GENERIC MAP(14) PORT MAP(X_inv, T0, Sum01);
    SM1 : SUM_GEN GENERIC MAP(14) PORT MAP(Sum01, X_inv4, Sum02);--14
    --M0_trunc<=Sum02(13)&Sum02(11 downto 0)&"00000"; --18
    --M0_trunc<=Sum02(13)&Sum02(13)&Sum02(11 downto 0)&"0000"; --quase
    M0_trunc <= Sum02(13) & Sum02(13) & Sum02(13) & Sum02(11 DOWNTO 0) & "000";

    --"001100101101000001", "001111001110110111", "001000010011011011", "000001110001101001"
    --A0 "001100101101000001"
    A0_aux0 <= Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1 DOWNTO 0);
    A0_aux6 <= Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1 DOWNTO 0) & "000000";
    A0_aux8 <= Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1 DOWNTO 0) & "00000000";
    A0_aux9 <= Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1 DOWNTO 0) & "000000000";
    A0_aux11 <= Y0(N - 1) & Y0(N - 1) & Y0(N - 1) & Y0(N - 1 DOWNTO 0) & "00000000000";
    A0_aux14 <= Y0(N - 1) & Y0(N - 2 DOWNTO 0) & "00000000000000";
    A0_aux15 <= Y0(N - 2 DOWNTO 0) & "000000000000000";

    SM_A00 : SUM_GEN GENERIC MAP(32) PORT MAP(A0_aux0, A0_aux6, SumA00);
    SM_A01 : SUM_GEN GENERIC MAP(32) PORT MAP(A0_aux8, A0_aux9, SumA01);
    SM_A02 : SUM_GEN GENERIC MAP(32) PORT MAP(A0_aux11, A0_aux14, SumA02);
    SM_A03 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA00, A0_aux15, SumA03);
    SM_A04 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA01, SumA02, SumA04);
    SM_A05 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA04, SumA03, SumA05);

    --A1 "001111001110110111" -A1= "110000110001001001"
    A1_aux0 <= Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1 DOWNTO 0);
    A1_aux3 <= Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1 DOWNTO 0) & "000";
    A1_aux6 <= Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1 DOWNTO 0) & "000000";
    A1_aux10 <= Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1 DOWNTO 0) & "0000000000";
    A1_aux11 <= Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1 DOWNTO 0) & "00000000000";
    A1_aux16 <= Y1(N - 3 DOWNTO 0) & "0000000000000000";
    A1_aux17 <= Y1(N - 4 DOWNTO 0) & "00000000000000000";

    SM_A10 : SUM_GEN GENERIC MAP(32) PORT MAP(A1_aux0, A1_aux3, SumA10);
    SM_A11 : SUM_GEN GENERIC MAP(32) PORT MAP(A1_aux6, A1_aux10, SumA11);
    SM_A12 : SUM_GEN GENERIC MAP(32) PORT MAP(A1_aux11, A1_aux16, SumA12);
    TtA3 : twoscompliment GENERIC MAP(32) PORT MAP(A1_aux17, TA1_aux17);
    SM_A13 : SUM_GEN GENERIC MAP(32) PORT MAP(TA1_aux17, SumA10, SumA13);
    SM_A14 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA11, SumA12, SumA14);
    SM_A15 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA14, SumA13, SumA15);

    --A2 "001000010011011011"
    A2_aux0 <= Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1 DOWNTO 0);
    A2_aux1 <= Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1 DOWNTO 0) & '0';
    A2_aux3 <= Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1 DOWNTO 0) & "000";
    A2_aux4 <= Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1 DOWNTO 0) & "0000";
    A2_aux6 <= Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1 DOWNTO 0) & "000000";
    A2_aux7 <= Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1 DOWNTO 0) & "0000000";
    A2_aux10 <= Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1) & Y2(N - 1 DOWNTO 0) & "0000000000";
    A2_aux15 <= Y2(N - 2 DOWNTO 0) & "000000000000000";

    SM_A20 : SUM_GEN GENERIC MAP(32) PORT MAP(A2_aux0, A2_aux1, SumA20);
    SM_A21 : SUM_GEN GENERIC MAP(32) PORT MAP(A2_aux3, A2_aux4, SumA21);
    SM_A22 : SUM_GEN GENERIC MAP(32) PORT MAP(A2_aux6, A2_aux7, SumA22);
    SM_A23 : SUM_GEN GENERIC MAP(32) PORT MAP(A2_aux10, A2_aux15, SumA23);
    SM_A24 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA21, SumA22, SumA24);
    SM_A25 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA24, SumA23, SumA25);
    SM_A26 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA25, SumA20, SumA26);

    --A3 "000001110001101001"
    A3_aux0 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 0);
    A3_aux3 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 0) & "000";
    A3_aux5 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 0) & "00000";
    A3_aux6 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 0) & "000000";
    A3_aux10 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 0) & "0000000000";
    A3_aux11 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 0) & "00000000000";
    A3_aux12 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 0) & "000000000000";

    SM_A30 : SUM_GEN GENERIC MAP(32) PORT MAP(A3_aux0, A3_aux3, SumA30);
    SM_A31 : SUM_GEN GENERIC MAP(32) PORT MAP(A3_aux5, A3_aux6, SumA31);
    SM_A32 : SUM_GEN GENERIC MAP(32) PORT MAP(A3_aux10, A3_aux11, SumA32);
    SM_A33 : SUM_GEN GENERIC MAP(32) PORT MAP(A3_aux12, SumA30, SumA33);
    SM_A34 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA31, SumA32, SumA34);
    SM_A35 : SUM_GEN GENERIC MAP(32) PORT MAP(SumA34, SumA33, SumA35);

    --Tt1: twoscompliment generic map(18) port map(M1_tr, T1);
    SumA05_aux <= SumA05(31 DOWNTO 14);
    SumA15_aux <= SumA15(31 DOWNTO 14);
    SumA35_aux <= SumA35(31 DOWNTO 14);
    SumA26_aux <= SumA26(31 DOWNTO 14);
    SM2 : SUM_GEN GENERIC MAP(18) PORT MAP(SumA05_aux, SumA15_aux, Sum03);
    Tt2 : twoscompliment GENERIC MAP(18) PORT MAP(SumA35_aux, T2);
    SM3 : SUM_GEN GENERIC MAP(18) PORT MAP(T2, SumA26_aux, Sum04);
    SM4 : SUM_GEN GENERIC MAP(18) PORT MAP(Sum03, Sum04, Sum05);
    SM5 : SUM_GEN GENERIC MAP(18) PORT MAP(Sum05, M0_trunc, Sum06);
    S <= Y0;
    --derivation
    X0_1_2 <= Sum06(N - 1) & Sum06(N - 1) & Sum06(N - 1 DOWNTO 2);
    X1_1_4 <= Y1(N - 1) & Y1(N - 1) & Y1(N - 1) & Y1(N - 1 DOWNTO 3);
    X3_1_2 <= Y3(N - 1) & Y3(N - 1) & Y3(N - 1) & Y3(N - 1 DOWNTO 3);
    X4_1_4 <= Y4(N - 1) & Y4(N - 1) & Y4(N - 1 DOWNTO 2);
    SM6 : SUM_GEN GENERIC MAP(18) PORT MAP(X0_1_2, X1_1_4, Sum07);
    SM7 : SUM_GEN GENERIC MAP(18) PORT MAP(X3_1_2, X4_1_4, Sum08);
    Tt3 : twoscompliment GENERIC MAP(18) PORT MAP(Sum08, T3);
    SM8 : SUM_GEN GENERIC MAP(18) PORT MAP(Sum07, T3, Sum09);
    S_der <= Sum09;
END comportamento;

--===============================integrador--======================================

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_signed.ALL;
USE work.comp_gen.ALL;
--USE work.comp_somadores.all;
-----------------------------------------------------
ENTITY integrador_par IS
    GENERIC (
        N : INTEGER := 32);
    --generic (N_C:integer:=4); --50 funciona
    PORT (
        reset, clock, Ld : IN STD_LOGIC;
        X : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        S : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0));
END integrador_par;

-----------------------------------------------------
ARCHITECTURE comportamento OF integrador_par IS

    SIGNAL X1, X2, X3, X4, X5, X6, X7, X8, X9, X10 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL X11, X12, X13, X14, X15, X16, X17, X18, X19, X20 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL X21, X22, X23, X24, X25, X26, X27, X28, X29, X30 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL X31, X32, X33 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL T0, T1 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL T11, T12, T13, T14, T15 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL SUM1, SUM2, SUM3 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
BEGIN

    --====================================================
    -- 
    -- h(n)= y(n) + y(n-1)+ y(n-2)+ y(n-3)+ y(n-4)

    --- Registrador de deslocamento para salvar valores passados da entrada X
    R1 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X, X1);
    R2 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X1, X2); -- 
    R3 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X2, X3); -- 
    R4 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X3, X4);
    R5 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X4, X5); -- 
    R6 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X5, X6); -- 
    R7 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X6, X7);
    R8 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X7, X8); -- 
    R9 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X8, X9); -- 
    R10 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X9, X10);
    R11 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X10, X11); -- 
    R12 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X11, X12); -- 
    R13 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X12, X13);
    R14 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X13, X14); -- 
    R15 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X14, X15); -- 
    R16 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X15, X16);
    R17 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X16, X17); -- 
    R18 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X17, X18); -- 
    R19 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X18, X19);
    R20 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X19, X20); -- 
    R21 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X20, X21); -- 
    R22 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X21, X22);
    R23 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X22, X23); -- 
    R24 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X23, X24); -- 
    R25 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X24, X25);
    R26 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X25, X26); -- 
    R27 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X26, X27); -- 
    R28 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X27, X28);
    R29 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X28, X29); -- 
    R30 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X29, X30);
    R31 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X30, X31); -- 
    R32 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X31, X32);

    R33 : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, X32, X33); -- 

    RAC_i : REG_GEN GENERIC MAP(N) PORT MAP(clock, Ld, reset, SUM1, SUM2);
    SM0 : SUM_GEN GENERIC MAP(N) PORT MAP(SUM2, X1, T0);
    c2_1 : twoscompliment GENERIC MAP(N) PORT MAP(X33, T1);
    SM3 : SUM_GEN GENERIC MAP(N) PORT MAP(T0, T1, SUM1);

    S <= SUM2;

END comportamento;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE comp_gen3 IS

    COMPONENT training_02 IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            reset, Ld_treino : IN STD_LOGIC;
            ecg_m, ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1 : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT conta_beat_6 IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            load : IN STD_LOGIC;
            ecg_m, ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            out_THR_SIG, out_SIG_LEV, out_NOISE_LEV : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0); --out_THR_NOISE
            out_THR_SIG1, out_SIG_LEV1, out_NOISE_LEV1, out_THR_NOISE1 : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            out_valor_arm, out_valor_arm_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            out_position_h : OUT INTEGER RANGE 0 TO 4000;
            --out_put : out std_logic_vector(N-1 downto 0)
            r_peak_number : OUT INTEGER RANGE 0 TO 4000
            --count : out integer range 0 to 1000
        );
    END COMPONENT;

    COMPONENT pass IS
        GENERIC (
            N : INTEGER := 18
            --K : INTEGER := 16
            );
        PORT (
            clear, clk, ld_l : IN STD_LOGIC;
            X : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
            --A1, A2, A3, A4: IN STD_LOGIC_VECTOR(17 DOWNTO 0);
            S, S_der : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0));
    END COMPONENT;

    COMPONENT integrador_par IS --paralelo
        GENERIC (N : INTEGER := 32);
        --generic (N_C:integer:=4); --50 funciona
        PORT (
            reset, clock, Ld : IN STD_LOGIC;
            X : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            S : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0));
    END COMPONENT;
END comp_gen3;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
--USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;
USE work.comp_gen.ALL;
USE work.comp_gen2.ALL;
USE work.comp_gen3.ALL;

ENTITY qrs_detector IS
    GENERIC (N : INTEGER := 18);
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        load : IN STD_LOGIC;
        ecg_m, ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        --THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        --THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_THR_SIG, out_SIG_LEV, out_NOISE_LEV : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0); --out_THR_NOISE
        out_THR_SIG1, out_SIG_LEV1, out_NOISE_LEV1, out_THR_NOISE1 : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_valor_arm, out_valor_arm_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_position_h : OUT INTEGER RANGE 0 TO 4000;
        --out_put : out std_logic_vector(N-1 downto 0)
        r_peak_number : OUT INTEGER RANGE 0 TO 4000
        --count : out integer range 0 to 1000
    );
END qrs_detector;

ARCHITECTURE Structural OF qrs_detector IS

    SIGNAL THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL THR_SIG_pip, SIG_LEV_pip, NOISE_LEV_pip, THR_NOISE_pip : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL THR_SIG1_pip, SIG_LEV1_pip, NOISE_LEV1_pip, THR_NOISE1_pip : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL sel, Ld_treino, Ld_conta : STD_LOGIC;
    SIGNAL cont : INTEGER := 0;

BEGIN
    treino : training_02 GENERIC MAP(N) PORT MAP(clk, reset, Ld_treino, ecg_m, ecg_h, THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE, THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1);
    pipeline_0 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, THR_SIG, THR_SIG_pip);
    pipeline_1 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, SIG_LEV, SIG_LEV_pip);
    pipeline_2 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, NOISE_LEV, NOISE_LEV_pip);
    pipeline_3 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, THR_NOISE, THR_NOISE_pip);
    pipeline_4 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, THR_SIG1, THR_SIG1_pip);
    pipeline_5 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, SIG_LEV1, SIG_LEV1_pip);
    pipeline_6 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, NOISE_LEV1, NOISE_LEV1_pip);
    pipeline_7 : REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld_treino, reset, THR_NOISE1, THR_NOISE1_pip);

    demux_clk : Demux1Bit2Out PORT MAP(load, sel, Ld_treino, Ld_conta);

    conta : conta_beat_6 GENERIC MAP(N) PORT MAP(clk, reset, Ld_conta, ecg_m, ecg_h, THR_SIG_pip, SIG_LEV_pip, NOISE_LEV_pip, THR_NOISE_pip, THR_SIG1_pip, SIG_LEV1_pip, NOISE_LEV1_pip, THR_NOISE1_pip, out_THR_SIG, out_SIG_LEV, out_NOISE_LEV, out_THR_SIG1, out_SIG_LEV1, out_NOISE_LEV1, out_THR_NOISE1, out_valor_arm, out_valor_arm_h, out_position_h, r_peak_number);

    PROCESS (clk, reset)

    BEGIN
        IF reset = '1' THEN
            cont <= 0;
            sel <= '0';
        ELSIF rising_edge(clk) THEN
            cont <= cont + 1;

            IF cont > 513 THEN
                sel <= '1';
            ELSE
                sel <= '0';
            END IF;
        END IF;
    END PROCESS;

END ARCHITECTURE Structural;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_signed.ALL;
USE work.comp_gen.ALL;
USE work.comp_gen2.ALL;
USE work.comp_gen3.ALL;
--USE work.comp_somadores.all;
-----------------------------------------------------
ENTITY PTA IS
    GENERIC (
        N : INTEGER := 18);
    --generic (N:integer:=32);
    PORT (
        reset, clk, Ld : IN STD_LOGIC; --clk06, clk20: IN STD_LOGIC;  
        X_in : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
        --A1, A2, A3, A4: IN STD_LOGIC_VECTOR(17 DOWNTO 0);
        S_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        S_integral : OUT STD_LOGIC_VECTOR(17 DOWNTO 0));
END PTA;

-----------------------------------------------------
ARCHITECTURE comportamento OF PTA IS

    SIGNAL S_interno_der : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    SIGNAL S_interno_der_pipe : STD_LOGIC_VECTOR(8 DOWNTO 0);
    SIGNAL s_interno_integral, S_interno_square_pipe : STD_LOGIC_VECTOR(17 DOWNTO 0);

BEGIN
    pass_band_der : pass GENERIC MAP(18) PORT MAP(reset, clk, Ld, X_in, S_h, S_interno_der);

    S_interno_der_pipe <= S_interno_der(16 DOWNTO 8);

    square : MULT_GEN GENERIC MAP(9) PORT MAP(S_interno_der_pipe, S_interno_der_pipe, s_interno_integral); --B1=1-

    S_interno_square_pipe <= '0' & s_interno_integral(17 DOWNTO 1); --1/32 moving 
    integral : integrador_par GENERIC MAP(18) PORT MAP(reset, clk, Ld, S_interno_square_pipe, S_integral);

END comportamento;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_signed.ALL;
USE work.comp_gen.ALL;

PACKAGE comp_gen4 IS

    COMPONENT PTA IS
        GENERIC (
            N : INTEGER := 18);
        --generic (N:integer:=32);
        PORT (
            reset, clk, Ld : IN STD_LOGIC; --clk06, clk20: IN STD_LOGIC;  
            X_in : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
            --A1, A2, A3, A4: IN STD_LOGIC_VECTOR(17 DOWNTO 0);
            S_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            S_integral : OUT STD_LOGIC_VECTOR(17 DOWNTO 0));
    END COMPONENT;

    COMPONENT qrs_detector IS
        GENERIC (N : INTEGER := 18);
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            load : IN STD_LOGIC;
            ecg_m, ecg_h : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            --THR_SIG, SIG_LEV, NOISE_LEV, THR_NOISE : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            --THR_SIG1, SIG_LEV1, NOISE_LEV1, THR_NOISE1 : IN STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            out_THR_SIG, out_SIG_LEV, out_NOISE_LEV : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0); --out_THR_NOISE
            out_THR_SIG1, out_SIG_LEV1, out_NOISE_LEV1, out_THR_NOISE1 : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            out_valor_arm, out_valor_arm_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
            out_position_h : OUT INTEGER RANGE 0 TO 4000;
            --out_put : out std_logic_vector(N-1 downto 0)
            r_peak_number : OUT INTEGER RANGE 0 TO 4000
            --count : out integer range 0 to 1000
        );
    END COMPONENT;

END comp_gen4;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
--USE IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.std_logic_arith.ALL;
USE work.comp_gen.ALL;
USE work.comp_gen2.ALL;
USE work.comp_gen3.ALL;
USE work.comp_gen4.ALL;

ENTITY pta_paralelo_qrs IS
    GENERIC (
        N : INTEGER := 18);
    --generic (N:integer:=32);
    PORT (
        reset, clk, Ld : IN STD_LOGIC; --clk06, clk20: IN STD_LOGIC;  
        X_in : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
        --A1, A2, A3, A4: IN STD_LOGIC_VECTOR(17 DOWNTO 0);
        --S_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        --S_integral : OUT STD_LOGIC_VECTOR(17 DOWNTO 0));
        out_THR_SIG, out_SIG_LEV, out_NOISE_LEV : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0); --out_THR_NOISE
        out_THR_SIG1, out_SIG_LEV1, out_NOISE_LEV1, out_THR_NOISE1 : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_valor_arm, out_valor_arm_h : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
        out_position_h : OUT INTEGER RANGE 0 TO 4000;
        --out_put : out std_logic_vector(N-1 downto 0)
        r_peak_number : OUT INTEGER RANGE 0 TO 4000;
        BPF_out, WI_out : OUT STD_LOGIC_VECTOR(N - 1 DOWNTO 0)
        --count : out integer range 0 to 1000
    );
END pta_paralelo_qrs;

ARCHITECTURE comportamento OF pta_paralelo_qrs IS
    SIGNAL filtro, integrador, temp1, temp2 : STD_LOGIC_VECTOR(N - 1 DOWNTO 0);
    --signal temp3 : integer; 
BEGIN

    --pta_paralelo : PTA GENERIC MAP(N) PORT MAP(reset, clk, Ld, X_in, filtro, integrador);
    pta_paralelo : PTA GENERIC MAP(N) PORT MAP(reset, clk, Ld, X_in, temp1, temp2);
    pipe_line_0: REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld, reset, temp1, filtro);
    pipe_line_1: REG_GEN GENERIC MAP(N) PORT MAP(clk, Ld, reset, temp2, integrador);
    qrs : qrs_detector GENERIC MAP(N) PORT MAP(clk, reset, Ld, integrador, filtro, out_THR_SIG, out_SIG_LEV, out_NOISE_LEV, out_THR_SIG1, out_SIG_LEV1, out_NOISE_LEV1, out_THR_NOISE1, out_valor_arm, out_valor_arm_h, r_peak_number);
    --pipe_out_r_peak: REG_GEN_INT GENERIC MAP(N) PORT MAP(clk, Ld, reset, temp3, r_peak_number);
    
    BPF_out <= filtro;
    WI_out <= integrador;            

END comportamento;