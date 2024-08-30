--definition of original constants :

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package sha256_constants is
    -- Initial hash values H(0)
    constant H0 : std_logic_vector(31 downto 0) := x"6a09e667";
    constant H1 : std_logic_vector(31 downto 0) := x"bb67ae85";
    constant H2 : std_logic_vector(31 downto 0) := x"3c6ef372";
    constant H3 : std_logic_vector(31 downto 0) := x"a54ff53a";
    constant H4 : std_logic_vector(31 downto 0) := x"510e527f";
    constant H5 : std_logic_vector(31 downto 0) := x"9b05688c";
    constant H6 : std_logic_vector(31 downto 0) := x"1f83d9ab";
    constant H7 : std_logic_vector(31 downto 0) := x"5be0cd19";

    -- Round constants K
    type k_array is array (0 to 63) of std_logic_vector(31 downto 0);
    constant K : k_array := (
        x"428a2f98", x"71374491", x"b5c0fbcf", x"e9b5dba5",
        x"3956c25b", x"59f111f1", x"923f82a4", x"ab1c5ed5",
        x"d807aa98", x"12835b01", x"243185be", x"550c7dc3",
        x"72be5d74", x"80deb1fe", x"9bdc06a7", x"c19bf174",
        x"e49b69c1", x"efbe4786", x"0fc19dc6", x"240ca1cc",
        x"2de92c6f", x"4a7484aa", x"5cb0a9dc", x"76f988da",
        x"983e5152", x"a831c66d", x"b00327c8", x"bf597fc7",
        x"c6e00bf3", x"d5a79147", x"06ca6351", x"14292967",
        x"27b70a85", x"2e1b2138", x"4d2c6dfc", x"53380d13",
        x"650a7354", x"766a0abb", x"81c2c92e", x"92722c85",
        x"a2bfe8a1", x"a81a664b", x"c24b8b70", x"c76c51a3",
        x"d192e819", x"d6990624", x"f40e3585", x"106aa070",
        x"19a4c116", x"1e376c08", x"2748774c", x"34b0bcb5",
        x"391c0cb3", x"4ed8aa4a", x"5b9cca4f", x"682e6ff3",
        x"748f82ee", x"78a5636f", x"84c87814", x"8cc70208",
        x"90befffa", x"a4506ceb", x"bef9a3f7", x"c67178f2"
    );

end package sha256_constants;

-------------------------------------------------------------------------------
--original functions:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package sha256_utilities is
    -- Word type (32-bit)
    subtype word is std_logic_vector(31 downto 0);
    
    -- Message block type (512-bit)
    subtype message_block is std_logic_vector(511 downto 0);
    
    -- Array of words (for W array in the main algorithm)
    type word_array is array (0 to 63) of word;
    
    -- Utility functions
    function rotr(x: word; n: natural) return word;
    function shr(x: word; n: natural) return word;
    
    -- SHA256 specific functions
    function ch(x, y, z: word) return word;
    function maj(x, y, z: word) return word;
    function sigma0(x: word) return word;
    function sigma1(x: word) return word;
    function big_sigma0(x: word) return word;
    function big_sigma1(x: word) return word;
end package sha256_utilities;

package body sha256_utilities is
    -- Rotate right
    function rotr(x: word; n: natural) return word is
    begin
        return std_logic_vector(rotate_right(unsigned(x), n));
    end function;
    
    -- Shift right
    function shr(x: word; n: natural) return word is
    begin
        return std_logic_vector(shift_right(unsigned(x), n));
    end function;
    
    -- Choose function
    function ch(x, y, z: word) return word is
    begin
        return (x and y) xor (not x and z);
    end function;
    
    -- Majority function
    function maj(x, y, z: word) return word is
    begin
        return (x and y) xor (x and z) xor (y and z);
    end function;
    
    -- Lower case sigma 0
    function sigma0(x: word) return word is
    begin
        return rotr(x, 7) xor rotr(x, 18) xor shr(x, 3);
    end function;
    
    -- Lower case sigma 1
    function sigma1(x: word) return word is
    begin
        return rotr(x, 17) xor rotr(x, 19) xor shr(x, 10);
    end function;
    
    -- Upper case sigma 0
    function big_sigma0(x: word) return word is
    begin
        return rotr(x, 2) xor rotr(x, 13) xor rotr(x, 22);
    end function;
    
    -- Upper case sigma 1
    function big_sigma1(x: word) return word is
    begin
        return rotr(x, 6) xor rotr(x, 11) xor rotr(x, 25);
    end function;
end package body sha256_utilities;

------------------------------------------------------------------------------------------
--original padding:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sha256_padding is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           data_in : in STD_LOGIC_VECTOR(511 downto 0);
           data_valid : in STD_LOGIC;
           last_block : in STD_LOGIC;
           message_length : in STD_LOGIC_VECTOR(63 downto 0);
           padded_data : out STD_LOGIC_VECTOR(511 downto 0);
           padded_valid : out STD_LOGIC);
end sha256_padding;

architecture Behavioral of sha256_padding is
    signal internal_data : STD_LOGIC_VECTOR(511 downto 0);
    signal padding_needed : STD_LOGIC;
    signal padding_state : INTEGER range 0 to 2 := 0;
begin
    process(clk, reset)
    begin
        if reset = '1' then
            internal_data <= (others => '0');
            padded_valid <= '0';
            padding_state <= 0;
            padding_needed <= '0';
        elsif rising_edge(clk) then
            case padding_state is
                when 0 =>
                    if data_valid = '1' then
                        internal_data <= data_in;
                        if last_block = '1' then
                            padding_needed <= '1';
                            padding_state <= 1;
                        else
                            padded_data <= data_in;
                            padded_valid <= '1';
                        end if;
                    else
                        padded_valid <= '0';
                    end if;
                
                when 1 =>
                    -- Add '1' bit and zeroes
                    internal_data(511 downto 448) <= internal_data(511 downto 448) & '1' & (446 downto 0 => '0');
                    -- Add message length
                    internal_data(63 downto 0) <= message_length;
                    padding_state <= 2;
                
                when 2 =>
                    padded_data <= internal_data;
                    padded_valid <= '1';
                    padding_state <= 0;
                    padding_needed <= '0';
            end case;
        end if;
    end process;
end Behavioral;
--------------------------------------------------------------------------------------------------------------------
--original parsing:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sha256_parser is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           data_in : in STD_LOGIC_VECTOR(31 downto 0);
           data_valid : in STD_LOGIC;
           last_word : in STD_LOGIC;
           block_ready : out STD_LOGIC;
           message_block : out STD_LOGIC_VECTOR(511 downto 0));
end sha256_parser;

architecture Behavioral of sha256_parser is
    type state_type is (IDLE, RECEIVE, PAD, OUTPUT);
    signal state : state_type := IDLE;
    
    type message_array is array (0 to 15) of std_logic_vector(31 downto 0);
    signal block_buffer : message_array := (others => (others => '0'));
    
    signal word_count : integer range 0 to 16 := 0;
    signal total_bits : unsigned(63 downto 0) := (others => '0');
    
begin
    process(clk, reset)
    begin
        if reset = '1' then
            state <= IDLE;
            word_count <= 0;
            total_bits <= (others => '0');
            block_ready <= '0';
            message_block <= (others => '0');
            block_buffer <= (others => (others => '0'));
        elsif rising_edge(clk) then
            case state is
                when IDLE =>
                    if data_valid = '1' then
                        state <= RECEIVE;
                        block_buffer(0) <= data_in;
                        word_count <= 1;
                        total_bits <= total_bits + 32;
                    end if;
                    
                when RECEIVE =>
                    if data_valid = '1' then
                        block_buffer(word_count) <= data_in;
                        word_count <= word_count + 1;
                        total_bits <= total_bits + 32;
                        
                        if word_count = 15 or last_word = '1' then
                            state <= PAD;
                        end if;
                    end if;
                    
                when PAD =>
                    if last_word = '1' and word_count < 15 then
                        block_buffer(word_count) <= x"80000000";
                        word_count <= word_count + 1;
                        
                        if word_count < 14 then
                            state <= OUTPUT;
                        else
                            state <= IDLE;
                        end if;
                    elsif word_count = 15 or (last_word = '1' and word_count = 14) then
                        block_buffer(14) <= std_logic_vector(total_bits(63 downto 32));
                        block_buffer(15) <= std_logic_vector(total_bits(31 downto 0));
                        state <= OUTPUT;
                    else
                        state <= OUTPUT;
                    end if;
                    
                when OUTPUT =>
                    for i in 0 to 15 loop
                        message_block(511 - i*32 downto 480 - i*32) <= block_buffer(i);
                    end loop;
                    block_ready <= '1';
                    state <= IDLE;
                    word_count <= 0;
                    
            end case;
        end if;
    end process;
end Behavioral;
----------------------------------------------------------------------------------------
--message schedule:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sha256_message_schedule is
    Port ( 
        clk : in STD_LOGIC;
        reset : in STD_LOGIC;
        block_in : in STD_LOGIC_VECTOR(511 downto 0);
        block_valid : in STD_LOGIC;
        word_out : out STD_LOGIC_VECTOR(31 downto 0);
        word_index : out INTEGER range 0 to 63;
        schedule_done : out STD_LOGIC
    );
end sha256_message_schedule;

architecture Behavioral of sha256_message_schedule is
    type state_type is (IDLE, PROCESS_BLOCK);
    signal state : state_type := IDLE;
    
    type word_array is array (0 to 63) of std_logic_vector(31 downto 0);
    signal W : word_array := (others => (others => '0'));
    
    signal index : INTEGER range 0 to 63 := 0;

    -- Function to perform right rotation
    function ror(x : std_logic_vector(31 downto 0); n : integer) return std_logic_vector is
    begin
        return x(n-1 downto 0) & x(31 downto n);
    end function;
    
    -- SHA-256 specific functions
    function sigma0(x : std_logic_vector(31 downto 0)) return std_logic_vector is
    begin
        return ror(x, 7) xor ror(x, 18) xor std_logic_vector(shift_right(unsigned(x), 3));
    end function;
    
    function sigma1(x : std_logic_vector(31 downto 0)) return std_logic_vector is
    begin
        return ror(x, 17) xor ror(x, 19) xor std_logic_vector(shift_right(unsigned(x), 10));
    end function;

begin
    process(clk, reset)
        variable temp1, temp2 : std_logic_vector(31 downto 0);
    begin
        if reset = '1' then
            state <= IDLE;
            index <= 0;
            W <= (others => (others => '0'));
            word_out <= (others => '0');
            word_index <= 0;
            schedule_done <= '0';
        elsif rising_edge(clk) then
            case state is
                when IDLE =>
                    if block_valid = '1' then
                        state <= PROCESS_BLOCK;
                        index <= 0;
                        -- Initialize first 16 words from the input block
                        for i in 0 to 15 loop
                            W(i) <= block_in(511 - i*32 downto 480 - i*32);
                        end loop;
                    end if;
                    
                when PROCESS_BLOCK =>
                    if index < 64 then
                        if index >= 16 then
                            -- Extend the message schedule
                            temp1 := sigma1(W(index-2));
                            temp2 := sigma0(W(index-15));
                            W(index) <= std_logic_vector(unsigned(W(index-16)) + unsigned(W(index-7)) + unsigned(temp1) + unsigned(temp2));
                        end if;
                        
                        -- Output current word
                        word_out <= W(index);
                        word_index <= index;
                        
                        if index = 63 then
                            schedule_done <= '1';
                            state <= IDLE;
                        else
                            index <= index + 1;
                        end if;
                    end if;
            end case;
        end if;
    end process;
end Behavioral;
---------------------------------------------------------------------------------------
--compression function:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sha256_compression is
    Port ( 
        clk : in STD_LOGIC;
        reset : in STD_LOGIC;
        start : in STD_LOGIC;
        w_in : in STD_LOGIC_VECTOR(31 downto 0);
        h_in : in STD_LOGIC_VECTOR(255 downto 0);
        h_out : out STD_LOGIC_VECTOR(255 downto 0);
        done : out STD_LOGIC
    );
end sha256_compression;

architecture Behavioral of sha256_compression is
    type state_type is (IDLE, PROCESS_ROUND, FINALIZE);
    signal state : state_type := IDLE;

    type word_array is array (0 to 7) of std_logic_vector(31 downto 0);
    signal h_reg : word_array;
    signal a, b, c, d, e, f, g, h : std_logic_vector(31 downto 0);

    signal round : integer range 0 to 63 := 0;

    -- Constants K for SHA-256
    type k_array is array (0 to 63) of std_logic_vector(31 downto 0);
    constant K : k_array := (
        x"428a2f98", x"71374491", x"b5c0fbcf", x"e9b5dba5",
        x"3956c25b", x"59f111f1", x"923f82a4", x"ab1c5ed5",
        x"d807aa98", x"12835b01", x"243185be", x"550c7dc3",
        x"72be5d74", x"80deb1fe", x"9bdc06a7", x"c19bf174",
        x"e49b69c1", x"efbe4786", x"0fc19dc6", x"240ca1cc",
        x"2de92c6f", x"4a7484aa", x"5cb0a9dc", x"76f988da",
        x"983e5152", x"a831c66d", x"b00327c8", x"bf597fc7",
        x"c6e00bf3", x"d5a79147", x"06ca6351", x"14292967",
        x"27b70a85", x"2e1b2138", x"4d2c6dfc", x"53380d13",
        x"650a7354", x"766a0abb", x"81c2c92e", x"92722c85",
        x"a2bfe8a1", x"a81a664b", x"c24b8b70", x"c76c51a3",
        x"d192e819", x"d6990624", x"f40e3585", x"106aa070",
        x"19a4c116", x"1e376c08", x"2748774c", x"34b0bcb5",
        x"391c0cb3", x"4ed8aa4a", x"5b9cca4f", x"682e6ff3",
        x"748f82ee", x"78a5636f", x"84c87814", x"8cc70208",
        x"90befffa", x"a4506ceb", x"bef9a3f7", x"c67178f2"
    );

    -- Function to perform right rotation
    function ror(x : std_logic_vector(31 downto 0); n : integer) return std_logic_vector is
    begin
        return x(n-1 downto 0) & x(31 downto n);
    end function;

    -- SHA-256 specific functions
    function ch(x, y, z : std_logic_vector(31 downto 0)) return std_logic_vector is
    begin
        return (x and y) xor ((not x) and z);
    end function;

    function maj(x, y, z : std_logic_vector(31 downto 0)) return std_logic_vector is
    begin
        return (x and y) xor (x and z) xor (y and z);
    end function;

    function ep0(x : std_logic_vector(31 downto 0)) return std_logic_vector is
    begin
        return ror(x, 2) xor ror(x, 13) xor ror(x, 22);
    end function;

    function ep1(x : std_logic_vector(31 downto 0)) return std_logic_vector is
    begin
        return ror(x, 6) xor ror(x, 11) xor ror(x, 25);
    end function;

begin
    process(clk, reset)
        variable t1, t2 : std_logic_vector(31 downto 0);
    begin
        if reset = '1' then
            state <= IDLE;
            round <= 0;
            done <= '0';
            h_reg <= (others => (others => '0'));
        elsif rising_edge(clk) then
            case state is
                when IDLE =>
                    if start = '1' then
                        state <= PROCESS_ROUND;
                        round <= 0;
                        done <= '0';
                        -- Initialize working variables
                        for i in 0 to 7 loop
                            h_reg(i) <= h_in(255-i*32 downto 224-i*32);
                        end loop;
                        a <= h_in(255 downto 224);
                        b <= h_in(223 downto 192);
                        c <= h_in(191 downto 160);
                        d <= h_in(159 downto 128);
                        e <= h_in(127 downto 96);
                        f <= h_in(95 downto 64);
                        g <= h_in(63 downto 32);
                        h <= h_in(31 downto 0);
                    end if;

                when PROCESS_ROUND =>
                    -- Compute t1 and t2
                    t1 := std_logic_vector(unsigned(h) + unsigned(ep1(e)) + unsigned(ch(e, f, g)) + unsigned(K(round)) + unsigned(w_in));
                    t2 := std_logic_vector(unsigned(ep0(a)) + unsigned(maj(a, b, c)));

                    -- Update working variables
                    h <= g;
                    g <= f;
                    f <= e;
                    e <= std_logic_vector(unsigned(d) + unsigned(t1));
                    d <= c;
                    c <= b;
                    b <= a;
                    a <= std_logic_vector(unsigned(t1) + unsigned(t2));

                    if round = 63 then
                        state <= FINALIZE;
                    else
                        round <= round + 1;
                    end if;

                when FINALIZE =>
                    -- Compute the new hash values
                    h_reg(0) <= std_logic_vector(unsigned(h_reg(0)) + unsigned(a));
                    h_reg(1) <= std_logic_vector(unsigned(h_reg(1)) + unsigned(b));
                    h_reg(2) <= std_logic_vector(unsigned(h_reg(2)) + unsigned(c));
                    h_reg(3) <= std_logic_vector(unsigned(h_reg(3)) + unsigned(d));
                    h_reg(4) <= std_logic_vector(unsigned(h_reg(4)) + unsigned(e));
                    h_reg(5) <= std_logic_vector(unsigned(h_reg(5)) + unsigned(f));
                    h_reg(6) <= std_logic_vector(unsigned(h_reg(6)) + unsigned(g));
                    h_reg(7) <= std_logic_vector(unsigned(h_reg(7)) + unsigned(h));

                    state <= IDLE;
                    done <= '1';

            end case;
        end if;
    end process;

    -- Output the final hash value
    h_out <= h_reg(0) & h_reg(1) & h_reg(2) & h_reg(3) & h_reg(4) & h_reg(5) & h_reg(6) & h_reg(7);

end Behavioral;
-------------------------------------------------------------------------------------
--output generation:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sha256_output_generation is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           state_in : in STD_LOGIC_VECTOR(255 downto 0);
           hash_out : out STD_LOGIC_VECTOR(255 downto 0);
           done : out STD_LOGIC);
end sha256_output_generation;

architecture Behavioral of sha256_output_generation is
    signal internal_state : STD_LOGIC_VECTOR(255 downto 0);
    signal output_ready : STD_LOGIC := '0';
begin
    process(clk, reset)
    begin
        if reset = '1' then
            internal_state <= (others => '0');
            output_ready <= '0';
            done <= '0';
        elsif rising_edge(clk) then
            if output_ready = '0' then
                internal_state <= state_in;
                output_ready <= '1';
                done <= '0';
            else
                done <= '1';
            end if;
        end if;
    end process;

    hash_out <= internal_state;
end Behavioral;
-----------------------------------------------------------------
--wrapper:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sha256_wrapper is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           data_in : in STD_LOGIC_VECTOR(511 downto 0);
           start : in STD_LOGIC;
           hash_out : out STD_LOGIC_VECTOR(255 downto 0);
           done : out STD_LOGIC);
end sha256_wrapper;

architecture Behavioral of sha256_wrapper is
    -- Component declarations
    component sha256_compression is
        Port ( clk : in STD_LOGIC;
               reset : in STD_LOGIC;
               data_in : in STD_LOGIC_VECTOR(511 downto 0);
               state_in : in STD_LOGIC_VECTOR(255 downto 0);
               state_out : out STD_LOGIC_VECTOR(255 downto 0));
    end component;

    component sha256_output_generation is
        Port ( clk : in STD_LOGIC;
               reset : in STD_LOGIC;
               state_in : in STD_LOGIC_VECTOR(255 downto 0);
               hash_out : out STD_LOGIC_VECTOR(255 downto 0);
               done : out STD_LOGIC);
    end component;

    -- Internal signals
    signal compression_state_out : STD_LOGIC_VECTOR(255 downto 0);
    signal compression_done : STD_LOGIC;
    
    -- Initial state constants
    constant initial_state : STD_LOGIC_VECTOR(255 downto 0) := 
        X"6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19";
    
    -- State machine
    type state_type is (IDLE, COMPRESS, OUTPUT);
    signal current_state : state_type := IDLE;

    -- SHA-256 constants
    type words_array is array (0 to 63) of STD_LOGIC_VECTOR(31 downto 0);
    constant K : words_array := (
        X"428a2f98", X"71374491", X"b5c0fbcf", X"e9b5dba5",
        X"3956c25b", X"59f111f1", X"923f82a4", X"ab1c5ed5",
        X"d807aa98", X"12835b01", X"243185be", X"550c7dc3",
        X"72be5d74", X"80deb1fe", X"9bdc06a7", X"c19bf174",
        X"e49b69c1", X"efbe4786", X"0fc19dc6", X"240ca1cc",
        X"2de92c6f", X"4a7484aa", X"5cb0a9dc", X"76f988da",
        X"983e5152", X"a831c66d", X"b00327c8", X"bf597fc7",
        X"c6e00bf3", X"d5a79147", X"06ca6351", X"14292967",
        X"27b70a85", X"2e1b2138", X"4d2c6dfc", X"53380d13",
        X"650a7354", X"766a0abb", X"81c2c92e", X"92722c85",
        X"a2bfe8a1", X"a81a664b", X"c24b8b70", X"c76c51a3",
        X"d192e819", X"d6990624", X"f40e3585", X"106aa070",
        X"19a4c116", X"1e376c08", X"2748774c", X"34b0bcb5",
        X"391c0cb3", X"4ed8aa4a", X"5b9cca4f", X"682e6ff3",
        X"748f82ee", X"78a5636f", X"84c87814", X"8cc70208",
        X"90befffa", X"a4506ceb", X"bef9a3f7", X"c67178f2"
    );

    -- Functions
    function rotr(x : STD_LOGIC_VECTOR; n : integer) return STD_LOGIC_VECTOR is
    begin
        return x(n-1 downto 0) & x(x'high downto n);
    end function;

    function ch(x, y, z : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    begin
        return (x and y) xor ((not x) and z);
    end function;

    function maj(x, y, z : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    begin
        return (x and y) xor (x and z) xor (y and z);
    end function;

    function ep0(x : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    begin
        return rotr(x, 2) xor rotr(x, 13) xor rotr(x, 22);
    end function;

    function ep1(x : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    begin
        return rotr(x, 6) xor rotr(x, 11) xor rotr(x, 25);
    end function;

begin
    -- Compression function
    process(clk, reset)
        variable a, b, c, d, e, f, g, h : STD_LOGIC_VECTOR(31 downto 0);
        variable w : words_array;
        variable t1, t2 : STD_LOGIC_VECTOR(31 downto 0);
    begin
        if reset = '1' then
            compression_state_out <= (others => '0');
            compression_done <= '0';
        elsif rising_edge(clk) then
            if current_state = COMPRESS then
                -- Initialize working variables
                a := initial_state(255 downto 224);
                b := initial_state(223 downto 192);
                c := initial_state(191 downto 160);
                d := initial_state(159 downto 128);
                e := initial_state(127 downto 96);
                f := initial_state(95 downto 64);
                g := initial_state(63 downto 32);
                h := initial_state(31 downto 0);

                -- Prepare message schedule
                for i in 0 to 15 loop
                    w(i) := data_in(511 - i*32 downto 480 - i*32);
                end loop;

                for i in 16 to 63 loop
                    w(i) := STD_LOGIC_VECTOR(unsigned(w(i-16)) + unsigned(w(i-7)) +
                            unsigned(rotr(w(i-15), 7) xor rotr(w(i-15), 18) xor ('0' & w(i-15)(31 downto 1))) +
                            unsigned(rotr(w(i-2), 17) xor rotr(w(i-2), 19) xor ('0' & '0' & w(i-2)(31 downto 2))));
                end loop;

                -- Main loop
                for i in 0 to 63 loop
                    t1 := STD_LOGIC_VECTOR(unsigned(h) + unsigned(ep1(e)) + unsigned(ch(e, f, g)) + unsigned(K(i)) + unsigned(w(i)));
                    t2 := STD_LOGIC_VECTOR(unsigned(ep0(a)) + unsigned(maj(a, b, c)));
                    h := g;
                    g := f;
                    f := e;
                    e := STD_LOGIC_VECTOR(unsigned(d) + unsigned(t1));
                    d := c;
                    c := b;
                    b := a;
                    a := STD_LOGIC_VECTOR(unsigned(t1) + unsigned(t2));
                end loop;

                -- Compute intermediate hash value
                compression_state_out <= 
                    STD_LOGIC_VECTOR(unsigned(a) + unsigned(initial_state(255 downto 224))) &
                    STD_LOGIC_VECTOR(unsigned(b) + unsigned(initial_state(223 downto 192))) &
                    STD_LOGIC_VECTOR(unsigned(c) + unsigned(initial_state(191 downto 160))) &
                    STD_LOGIC_VECTOR(unsigned(d) + unsigned(initial_state(159 downto 128))) &
                    STD_LOGIC_VECTOR(unsigned(e) + unsigned(initial_state(127 downto 96))) &
                    STD_LOGIC_VECTOR(unsigned(f) + unsigned(initial_state(95 downto 64))) &
                    STD_LOGIC_VECTOR(unsigned(g) + unsigned(initial_state(63 downto 32))) &
                    STD_LOGIC_VECTOR(unsigned(h) + unsigned(initial_state(31 downto 0)));

                compression_done <= '1';
            else
                compression_done <= '0';
            end if;
        end if;
    end process;

    -- Output generation
    output_gen: sha256_output_generation
    port map (
        clk => clk,
        reset => reset,
        state_in => compression_state_out,
        hash_out => hash_out,
        done => done
    );

    -- State machine process
    process(clk, reset)
    begin
        if reset = '1' then
            current_state <= IDLE;
        elsif rising_edge(clk) then
            case current_state is
                when IDLE =>
                    if start = '1' then
                        current_state <= COMPRESS;
                    end if;
                when COMPRESS =>
                    if compression_done = '1' then
                        current_state <= OUTPUT;
                    end if;
                when OUTPUT =>
                    if done = '1' then
                        current_state <= IDLE;
                    end if;
            end case;
        end if;
    end process;

end Behavioral;